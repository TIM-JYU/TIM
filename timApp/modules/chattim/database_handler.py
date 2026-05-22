from dataclasses import dataclass

from sqlalchemy import select, delete
from typing import cast
from timApp.document.document import Document
from timApp.document import docentry
from timApp.document.docentry import DocEntry, get_documents_in_folder
from timApp.folder.folder import Folder, path_includes, get_documents
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
    def get_tim_document_by_id(doc_id: int) -> Document | None:
        """
        Returns a document corresponding to the given id.
        """
        doc_entry = TimDatabase.get_doc_entry_by_id(doc_id)
        if doc_entry:
            return doc_entry.document  # paragraphs -> .get_paragraphs()
        return None

    @staticmethod
    def get_tim_document_by_path(path: str) -> Document | None:
        doc = DocEntry.find_by_path(path)
        return doc.document if doc else None

    @staticmethod
    def get_tim_documents_by_path(path: str, recursive: bool = False) -> list[Document]:
        """
        Returns all documents in the given path recursively.
        If no documents are found, returns empty list.
        If path is a document the document is returned.
        If path is a folder all the documents in the folder are returned.
        If recursive is True, all subfolders are returned recursively.
        If path is neither a folder nor a document, returns empty list.
        """
        documents: list[Document] = []
        document = TimDatabase.get_tim_document_by_path(path)
        if document:
            documents.append(document)
            return documents

        doc_entries = docentry.get_documents(
            filter_folder=path, search_recursively=recursive
        )
        for d in doc_entries if doc_entries else []:
            documents.append(d.document)  # paragraphs -> .get_paragraphs()
        return documents

    @staticmethod
    def get_doc_entry_by_id(doc_id: int) -> DocEntry | None:
        entries = docentry.DocEntry.find_all_by_id(doc_id)
        if not entries:
            return None
        return entries[0]

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
    def get_user_groups(groups: list[str]) -> list[UserGroup]:
        """Check if all the user groups exists and return them."""
        user_groups: list[UserGroup] = []
        for group in groups:
            g = UserGroup.get_by_name(group)
            if not g:
                raise Exception(f"Invalid user group: {group}")
            user_groups.append(g)
        return user_groups

    @staticmethod
    def validate_item_paths(
        user: User,
        paths: list[str],
        *,
        depth: int = 1,
        add_documents: bool = True,
    ) -> list[Item]:
        """
        Check if all the paths exist and that the user has access to them.
        If the path is a folder, check access to the documents directly under it.

        :param user: The user of which access to check.
        :param paths: The list of paths to check.
        :param depth: The depth to check.
                      If depth > 0, check the documents under folders.
        :param add_documents: Whether to add documents to the returned item list.
        :return: The list of items found.
        """
        items: list[Item] = []
        to_check: list[str] = []

        for path in paths:
            item = Item.find_by_path(path)
            if not item:
                raise Exception(f"Invalid item path: '{path}'")

            is_folder = isinstance(item, Folder)

            rights = get_user_rights_for_item(item, user)
            if not rights.get("owner") and not rights.get("manage"):
                raise Exception(f"Insufficient rights: '{path}'")

            if not is_folder:
                if add_documents:
                    items.append(item)
                continue

            items.append(item)
            folder: Folder = cast(Folder, item)

            # Check rights for the documents under this folder
            if depth > 0:
                docs = get_documents_in_folder(path)
                to_check.extend([d.name for d in docs])
                # Check rights for the folders under this folder
                if depth > 1:
                    folders = folder.get_all_folders()
                    to_check.extend([f.path for f in folders])

        # Check the rights for pending items
        if len(to_check) > 0:
            sub_items = TimDatabase.validate_item_paths(
                user, to_check, add_documents=False, depth=depth - 1
            )
            items.extend(sub_items)

        return items

    @staticmethod
    def api_key_valid_in_doc(key: LLMRule, doc_id: int) -> bool:
        """Check if the API key can be used in the given document."""
        if key.document_id > 0:
            raise ValueError("Not an API key")
        if len(key.paths) == 0:
            raise PermissionError("API key is not valid in any document.")

        for path in key.paths:
            item = Item.find_by_path(path)
            if not item:
                continue
            is_doc = isinstance(item, DocEntry)

            if is_doc:
                doc = cast(DocEntry, item)
                if doc.id == doc_id:
                    return True
                continue

            # Check if the document is this folder
            folder = cast(Folder, item)
            docs = folder.get_all_documents()
            # TODO: can probably be done more efficiently
            for doc_info in docs:
                if doc_info.document.doc_id == doc_id:
                    return True
        entry = TimDatabase.get_doc_entry_by_id(doc_id)
        if entry is None:
            raise ValueError(f"No document with ID {doc_id}")
        raise PermissionError(f"API key has no access to document '{entry.path}'")

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

        if not group_ids:
            return None
        for group_id in group_ids:
            group = UserGroup.get_by_id(group_id)
            if not group:
                continue
            if TimDatabase.in_user_group(group, user_id):
                return api_key
        return None

    @staticmethod
    def create_plugin(document_id: int, owner: int) -> LLMRule:
        rule = LLMRule(document_id=document_id, owner=owner)
        db.session.add(rule)
        db.session.commit()
        return rule

    @staticmethod
    def set_llm_rule(
        document_id: int,
        owner: int,
        public_key: str,
        use_streaming: bool,
        temperature: float | None,
        include_citations: bool,
        similarity_threshold: float | None,
        top_k_chunks: int,
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
        :param include_citations: Whether to include citations.
        :param similarity_threshold: The similarity threshold for context inclusion.
        :param top_k_chunks: The number of top chunks to include.
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
                include_citations=include_citations,
                similarity_threshold=similarity_threshold,
                top_k_chunks=top_k_chunks,
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
            rule.public_key = public_key
            rule.teachers = teachers
            rule.current_mode = current_mode
            rule.total_tokens_spent = total_tokens_spent
            rule.indexed_document_ids = indexed_document_ids
            rule.agent = agent
            rule.conv_time_window = conv_time_window
            rule.system_prompt_path = system_prompt_path
            rule.use_streaming = use_streaming
            rule.temperature = temperature
            rule.include_citations = include_citations
            rule.similarity_threshold = similarity_threshold
            rule.top_k_chunks = top_k_chunks
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
    ) -> LLMRule:
        """Update the API key permissions."""
        rule = TimDatabase.get_owner_api_key(owner_id, alias)
        if not rule:
            raise Exception("No API-key with the alias found")

        user = User.get_by_id(owner_id)
        if not user:
            raise Exception(f"Invalid user.")

        user_groups = TimDatabase.get_user_groups(groups)
        TimDatabase.validate_item_paths(user, paths)

        # Append unique user groups
        group_ids: set[int] = set(rule.groups)
        for group in user_groups:
            group_ids.add(group.id)

        rule.groups = list(group_ids)
        rule.paths = paths
        db.session.commit()
        return rule

    @staticmethod
    def remove_api_key_group(owner_id: int, public_key: str, group_id: int) -> None:
        """Remove access to the API key from the given user group."""
        rule = TimDatabase.get_owner_api_key(owner_id, public_key)
        if not rule:
            raise Exception("No API-key with the alias found")
        groups = rule.groups
        rule.groups = filter(lambda gid: gid != group_id, groups)  # type: ignore
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
            if token_time_window_type is not None:
                policy.token_time_window_type = token_time_window_type
            if token_time_window_num is not None:
                policy.token_time_window_num = token_time_window_num
            if time_window_tokens is not None:
                policy.time_window_tokens = time_window_tokens
            if max_tokens_per_user is not None:
                policy.max_tokens_per_user = max_tokens_per_user
            if token_pool is not None:
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
            Policy.llm_rule_id == llm_rule.id,
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
            Policy.llm_rule_id == llm_rule.id,
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
            Policy.llm_rule_id == llm_rule.id,
            Policy.for_user.is_(None),
        )
        return db.session.scalar(stmt)

    @staticmethod
    def get_user_policy(llm_rule: LLMRule, user_id: int) -> Policy | None:
        """
        Gets the user policy of the given user.
        """
        stmt = select(Policy).where(
            Policy.llm_rule_id == llm_rule.id,
            Policy.for_user == user_id,
        )
        return db.session.scalar(stmt)

    @staticmethod
    def set_usage(user: int, llm_rule: LLMRule, used_tokens: int) -> Usage:
        """
        Sets the usage for the given user in the given LLM rule context.
        :param user: ID of the user for the usage.
        :param llm_rule: LLM rule instance.
        :param used_tokens: Number of used tokens.
        :return: Usage of the given user.
        """
        usage = TimDatabase.get_usage(llm_rule, user)
        if not usage:
            usage = Usage(
                user=user,
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
            Usage.llm_rule_id == llm_rule.id,
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
            Usage.llm_rule_id == llm_rule.id,
            Usage.user == user_id,
        )
        return db.session.scalar(stmt)

    @staticmethod
    def get_shared_api_keys(user_id: int) -> list[LLMRule]:
        """Get API keys shared with the user via user groups."""
        user = User.get_by_id(user_id)
        if not user:
            return []
        user_group_ids = [g.id for g in user.groups]
        if not user_group_ids:
            return []

        stmt = select(LLMRule).where(
            LLMRule.owner != user_id,
            LLMRule.document_id <= 0,
            LLMRule.groups.overlap(user_group_ids),
        )
        return db.session.execute(stmt).scalars().all()

    @staticmethod
    def get_usages(llm_rule: LLMRule) -> list[Usage] | None:
        """
        Gets all usages associated with llm rule.
        :param llm_rule:
        :return:
        """
        stmt = select(Usage).where(
            Usage.llm_rule == llm_rule,
        )
        return db.session.scalars(stmt).all()
