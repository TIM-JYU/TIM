from dataclasses import dataclass

from sqlalchemy import select, delete

from timApp.document.document import Document
from timApp.document import docentry
from timApp.document.docentry import DocEntry
from timApp.folder.folder import Folder, path_includes
from timApp.item.item import Item
from timApp.modules.chattim.dbmodels import LLMRule, Policy, Usage
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.auth.get_user_rights_for_item import (
    get_user_rights_for_item,
    UserItemRights,
)


@dataclass
class TimDatabase:
    @staticmethod
    def identify_user(user_id: int) -> str:
        """
        Checks if the given user is a teacher or a student
        """
        user = User.get_by_id(user_id)
        if user:
            teacher = user.is_sisu_teacher
            if teacher:
                return "teacher"
            student = User.get_home_org_student_id(user)
            if student:
                return "student"
        return "user"  # TODO: better options

    @staticmethod
    def get_tim_document_by_id(doc_id: int) -> Document | None:
        """
        Returns a document corresponding to the given id.
        """
        doc_entry = docentry.DocEntry.find_all_by_id(doc_id)
        if doc_entry:
            return doc_entry[0].document  # paragraphs -> .get_paragraphs()
        return None

    @staticmethod
    def get_tim_document_by_path(path: str) -> Document | None:
        doc = DocEntry.find_by_path(path)
        return doc.document if doc else None

    @staticmethod
    def get_tim_documents_by_path(path: str) -> list[Document]:
        """
        Returns all documents in the given path recursively.
        If no documents are found, returns empty list.
        If path is a document the document is returned.
        If path is a folder all the documents in the folder and it's subfolders are returned recursively.
        If path is neither a folder nor a document, returns empty list.
        """
        documents: list[Document] = []
        document = TimDatabase.get_tim_document_by_path(path)
        if document:
            documents.append(document)
            return documents

        doc_entries = docentry.get_documents(filter_folder=path)
        for d in doc_entries if doc_entries else []:
            documents.append(d.document)  # paragraphs -> .get_paragraphs()
        return documents

    @staticmethod
    def check_rights(user_id: int, doc_id: int) -> UserItemRights | None:
        """
        Checks which rights the given user has for the given document or folder.
        """
        user = User.get_by_id(user_id)
        doc = Item.find_by_id(doc_id)
        if user and doc:
            rights = get_user_rights_for_item(doc, user)
            return rights
        else:
            return None

    @staticmethod
    def in_user_group(group: UserGroup, user_id: int) -> bool:
        """Check if the user is in the given user group."""
        # TODO: is there a TIM function for this?
        if group.is_personal_group and group.personal_user.id == user_id:
            return True
        return any(u.id == user_id for u in group.users)

    @staticmethod
    def validate_user_groups(groups: list[str]) -> list[UserGroup]:
        """Check if all the user groups exists."""
        user_groups: list[UserGroup] = []
        for group in groups:
            g = UserGroup.get_by_name(group)
            if not g:
                raise Exception(f"Invalid user group: {group}")
            user_groups.append(g)
        return user_groups

    @staticmethod
    def validate_item_paths(paths: list[str]) -> list[Item]:
        """Check if all the paths exist."""
        items: list[Item] = []
        for path in paths:
            item = Item.find_by_path(path)
            if not item:
                raise Exception(f"Invalid item path: {item}")
            items.append(item)
        return items

    @staticmethod
    def api_key_valid_in_doc(key: LLMRule, doc_id: int) -> bool:
        """Check if the API key can be used in the given document."""
        if key.document_id > 0:
            return False
        # TODO: Should the key be accessible everywhere if no paths added or no?
        if len(key.paths) == 0:
            return False

        entries = docentry.DocEntry.find_all_by_id(doc_id)
        if not entries:
            return False
        doc = entries[0]

        for path in key.paths:
            if doc.path == path:
                return True
            item = Item.find_by_path(path)
            if not item:
                continue
            if isinstance(item, Folder):
                # TODO: is there a better way?
                if path_includes(doc.path, path):
                    return True
        return False

    @staticmethod
    def access_api_key(user_id: int, public_key: str) -> LLMRule | None:
        """
        Get the API key if the given user has access to it.
        User has access to the key if it's the owner of the key, or
        it is included in some of the user groups that have access.

        :param user_id: The id of the user.
        :param public_key: The associated public key for the desired API key.
        :return: The API key or None if it does not exist or the user has no access.
        """
        api_key = TimDatabase.get_api_key_by_alias(public_key)
        if not api_key:
            return None
        if user_id == api_key.owner:
            return api_key
        group_ids = api_key.groups
        # TODO: If the user group list is empty,
        #  should the key be global or accessible only to the owner?
        if not group_ids:
            return api_key
        for group_id in group_ids:
            group = UserGroup.get_by_id(group_id)
            if not group:
                continue
            if TimDatabase.in_user_group(group, user_id):
                return api_key
        return None

    @staticmethod
    def set_llm_rule(
        document_id: int,
        owner: int,
        public_key: str,
        use_streaming: bool,
        temperature: float | None,
        teachers: list[int],
        current_mode: str,
        total_tokens_spent: int,
        indexed_document_ids: list[int],
        system_prompt_path: str,
        agent: str,
        conv_time_window: int,
        policy: list[Policy],
        usage: list[Usage],
    ) -> LLMRule:
        """
        Creates a new LLM rule or updates the existing rule if one already exists for the given owner.
        :param document_id: The id of the document.
        :param owner: The id of the owner of the LLM rule.
        :param public_key: The alias of the chosen API key for the instance.
        :param use_streaming: Use streaming for model answers.
        :param temperature: The temperature parameter for the model.
        :param teachers: The ids of the teachers allowed to use the plugin instance.
        :param current_mode: Mode of the plugin instance: summarizing, creative or balanced.
        :param total_tokens_spent: The total number of tokens spent.
        :param indexed_document_ids:
        :param system_prompt_path: TIM path for an optional custom system prompt.
        :param agent: LLm agent.
        :param conv_time_window: Time window for the conversation in minutes.
        :param policy: List of policies related to the LLMRule instance.
        :param usage: List of usages related to the LLMRule instance.
        :return: created LLMRule instance
        """
        rule = TimDatabase.get_llm_rule(document_id)
        if not rule:
            rule = LLMRule(
                document_id=document_id,
                owner=owner,
                public_key=public_key,
                use_streaming=use_streaming,
                temperature=temperature,
                teachers=teachers,
                current_mode=current_mode,
                total_tokens_spent=total_tokens_spent,
                indexed_document_ids=indexed_document_ids,
                system_prompt_path=system_prompt_path,
                agent=agent,
                conv_time_window=conv_time_window,
            )
            db.session.add(rule)
        else:
            if public_key:
                rule.public_key = public_key
            if teachers:
                rule.teachers = teachers
            if current_mode:
                rule.current_mode = current_mode
            if total_tokens_spent:
                rule.total_tokens_spent = total_tokens_spent
            if indexed_document_ids:
                rule.indexed_chunk_ids = indexed_document_ids
            if agent:
                rule.agent = agent
            if conv_time_window:
                rule.conv_time_window = conv_time_window
            rule.system_prompt_path = system_prompt_path
            rule.use_streaming = use_streaming
            rule.temperature = temperature
        rule.policy.extend(policy)
        rule.usage.extend(usage)
        db.session.commit()
        return rule

    @staticmethod
    def set_api_key(
        owner: int,
        provider: str,
        public_key: str,
        api_key: str,
        *,
        group_names: list[str] | None = None,
        paths: list[str] | None = None,
    ) -> LLMRule:
        """
        Saves a new API key, its alias and the API key provider.
        :param owner: The id of the owner of the apikey.
        :param provider: Provider of the apikey.
        :param public_key: The public alias for the key.
        :param api_key: The API-key.
        :param group_names: Optional user groups to add to the API key permissions.
        :param paths: Optional paths to add to the API key permissions.
        :return: created LLMRule instance
        """
        rule = TimDatabase.get_api_key_by_alias(public_key)
        if rule:
            if owner != rule.owner:
                raise Exception("API-key exists with the alias.")
        else:
            rule = LLMRule(owner=owner)
            db.session.add(rule)

        rule.provider = provider
        rule.api_key = api_key
        rule.public_key = public_key

        if group_names:
            groups: list[int] = []
            for group_name in group_names:
                if g := UserGroup.get_by_name(group_name):
                    groups.append(g.id)
            rule.groups = groups
        if paths:
            rule.paths = paths

        db.session.commit()
        return rule

    @staticmethod
    def delete_llm_rule(owner_id: int, document_id: int) -> None:
        """
        Deletes the LLM rule of the given owner in the given document.
        """
        stmt = delete(LLMRule).where(
            LLMRule.owner == owner_id,
            LLMRule.document_id == document_id,
        )
        db.session.execute(stmt)
        db.session.commit()

    @staticmethod
    def get_llm_rule(document_id: int) -> LLMRule | None:
        """
        Gets the LLM rule in the given document.
        """
        stmt = select(LLMRule).where(LLMRule.document_id == document_id)
        return db.session.scalar(stmt)

    @staticmethod
    def get_user_api_keys(owner_id: int) -> list[LLMRule]:
        """Get all the API keys owner by the given owner."""
        stmt = (
            select(LLMRule)
            .where(LLMRule.owner == owner_id, LLMRule.document_id <= 0)
            .order_by(LLMRule.id)
        )
        return db.session.execute(stmt).scalars().all()

    @staticmethod
    def get_api_key_by_alias(public_key: str) -> LLMRule | None:
        """Gets the LLM rule table based on the alias."""
        stmt = select(LLMRule).where(
            LLMRule.document_id <= 0, LLMRule.public_key == public_key
        )
        return db.session.scalar(stmt)

    @staticmethod
    def get_owner_api_key(owner_id: int, alias: str) -> LLMRule | None:
        """Gets the LLM rule row of the owner based on the alias."""
        stmt = select(LLMRule).where(
            LLMRule.owner == owner_id,
            LLMRule.document_id <= 0,
            LLMRule.public_key == alias,
        )
        return db.session.scalar(stmt)

    @staticmethod
    def delete_api_key(owner_id: int, alias: str) -> None:
        """Deletes the LLM rule row of the owner based on the alias."""
        key = TimDatabase.get_owner_api_key(owner_id, alias)
        if not key:
            raise Exception("No API key with the alias found.")
        assert isinstance(key, LLMRule)
        db.session.delete(key)
        db.session.commit()

    @staticmethod
    def update_api_key_permissions(
        owner_id: int, alias: str, groups: list[str], paths: list[str]
    ) -> None:
        """Gets the LLM rule table based on the alias."""
        user_groups = TimDatabase.validate_user_groups(groups)
        TimDatabase.validate_item_paths(paths)

        rule = TimDatabase.get_owner_api_key(owner_id, alias)
        if not rule:
            raise Exception("No API-key with the alias found")

        rule.groups = [g.id for g in user_groups]
        rule.paths = paths
        db.session.commit()

    @staticmethod
    def set_policy(
        user: int,
        llm_rule: LLMRule,
        token_time_window_type: str | None,
        token_time_window_num: int | None,
        time_window_tokens: int | None,
        max_tokens_per_user: int | None,
        token_pool: int | None,
        policy_type: str,  # global or user
    ) -> Policy:
        """
        Sets a new policy for the LLM rule. Policy can be a global policy for the whole LLM rule or a student policy
        for the given user.
        """
        if user:
            policy = TimDatabase.get_user_policy(llm_rule, user)
        else:
            policy = TimDatabase.get_global_policy(llm_rule)
        if not policy:
            policy = Policy(
                for_user=user,
                llm_rule_id=llm_rule.id,
                llm_rule=llm_rule,
                token_time_window_type=token_time_window_type,
                token_time_window_num=token_time_window_num,
                time_window_tokens=time_window_tokens,
                max_tokens_per_user=max_tokens_per_user,
                token_pool=token_pool,
                policy_type=policy_type,
            )
            db.session.add(policy)
        else:
            if token_time_window_type:
                policy.token_time_window_type = token_time_window_type
            if token_time_window_num:
                policy.token_time_window_num = token_time_window_num
            if time_window_tokens:
                policy.time_window_tokens = time_window_tokens
            if max_tokens_per_user:
                policy.max_tokens_per_user = max_tokens_per_user
            if token_pool:
                policy.token_pool = token_pool
        db.session.commit()
        return policy

    def set_global_policy(
        self,
        llm_rule: LLMRule,
        token_time_window_type: str | None,
        token_time_window_num: int | None,
        time_window_tokens: int | None,
        max_tokens_per_user: int | None,
        token_pool: int | None,
    ) -> Policy:
        """
        Sets the global policy for the LLM rule.
        :return:
        """
        policy = self.set_policy(
            0,
            llm_rule,
            token_time_window_type,
            token_time_window_num,
            time_window_tokens,
            max_tokens_per_user,
            token_pool,
            "global",
        )
        return policy

    def set_user_policy(
        self,
        user: int,
        llm_rule: LLMRule,
        token_time_window_type: str | None,
        token_time_window_num: int | None,
        time_window_tokens: int | None,
        max_tokens_per_user: int | None,
    ) -> Policy:
        """
        Sets the user policy for the given user in the LLM rule context.
        :return:
        """
        policy = self.set_policy(
            user,
            llm_rule,
            token_time_window_type,
            token_time_window_num,
            time_window_tokens,
            max_tokens_per_user,
            0,
            "user",
        )
        return policy

    @staticmethod
    def delete_policy(policy_id: int) -> None:
        """
        Deletes the given policy.
        """
        stmt = delete(Policy).where(Policy.id == policy_id)
        db.session.execute(stmt)
        db.session.commit()

    @staticmethod
    def delete_global_policy(llm_rule: LLMRule) -> None:
        """
        Deletes the given global policy.
        """
        stmt = delete(Policy).where(
            Policy.llm_rule == llm_rule,
            Policy.for_user.is_(None),
        )
        db.session.execute(stmt)
        db.session.commit()

    @staticmethod
    def delete_user_policy(llm_rule: LLMRule, user_id: int) -> None:
        """
        Deletes the user policy of the given user.
        """
        stmt = delete(Policy).where(
            Policy.llm_rule == llm_rule,
            Policy.for_user == user_id,
        )
        db.session.execute(stmt)
        db.session.commit()

    @staticmethod
    def get_global_policy(llm_rule: LLMRule) -> Policy | None:
        """
        Gets the given global policy.
        """
        stmt = select(Policy).where(
            Policy.llm_rule == llm_rule,
            Policy.for_user.is_(None),
        )
        return db.session.scalar(stmt)

    @staticmethod
    def get_user_policy(llm_rule: LLMRule, user_id: int) -> Policy | None:
        """
        Gets the user policy of the given user.
        """
        stmt = select(Policy).where(
            Policy.llm_rule == llm_rule,
            Policy.for_user == user_id,
        )
        return db.session.scalar(stmt)

    @staticmethod
    def set_usage(
        user: int, conv_id: int, llm_rule: LLMRule, used_tokens: int
    ) -> Usage:
        """
        Sets the usage for the given user in the given LLM rule context.
        :param user: ID of the user for the usage.
        :param conv_id: ID of the conversation of the user.
        :param llm_rule: LLM rule instance.
        :param used_tokens: Number of used tokens.
        :return: Usage of the given user.
        """
        usage = TimDatabase.get_usage(llm_rule, user)
        if not usage:
            usage = Usage(
                user=user,
                conversation_id=conv_id,
                llm_rule_id=llm_rule.id,
                llm_rule=llm_rule,
                used_tokens=used_tokens,
            )
            db.session.add(usage)
        else:
            usage.used_tokens = used_tokens
        db.session.commit()
        return usage

    @staticmethod
    def delete_usage(llm_rule: LLMRule, user_id: int) -> None:
        """
        Deletes the usage of the given user in the given LLM rule instance.
        """
        stmt = delete(Usage).where(
            Usage.llm_rule == llm_rule,
            Usage.user == user_id,
        )
        db.session.execute(stmt)
        db.session.commit()

    @staticmethod
    def get_usage(llm_rule: LLMRule, user_id: int) -> Usage | None:
        """
        Gets the usage of the given user in the given LLM rule instance.
        """
        stmt = select(Usage).where(
            Usage.llm_rule == llm_rule,
            Usage.user == user_id,
        )
        return db.session.scalar(stmt)
