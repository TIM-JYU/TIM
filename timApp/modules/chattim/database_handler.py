from dataclasses import dataclass

from sqlalchemy import select, delete

from timApp.document.document import Document
from timApp.document import docentry
from timApp.document.docentry import DocEntry
from timApp.item.item import Item
from timApp.modules.chattim.dbmodels import LLMRule, Policy, Usage
from timApp.timdb.sqa import db
from timApp.user.user import User
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
    def get_tim_documents_by_path(path: str) -> list[Document]:
        """
        Returns all documents in the given path recursively.
        If no documents are found, returns empty list.
        If path is a document the document is returned.
        If path is a folder all the documents in the folder and it's subfolders are returned recursively.
        If path is neither a folder nor a document, returns empty list.
        """
        documents = []
        doc = DocEntry.find_by_path(path)
        if doc:
            documents.append(doc.document)
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
    def create_llm_rule(
            document_id: int,
            owner: int,
            apikey_provider: str,
            apikey: list[str],
            chosen_key: str,
            teachers: list[int],
            current_mode: str,
            total_tokens_spent: int,
            indexed_chunk_ids: list[int],
            agent: str,
            conv_time_window: int,
            policy: list[Policy],
            usage: list[Usage],
    ) -> LLMRule:
        """
        Creates a new LLM rule.
        :param document_id: The id of the document.
        :param owner: The id of the owner.
        :param apikey_provider: The provider for the apikey.
        :param apikey: List of the api keys and aliases of the owner.
        :param chosen_key: The chosen apikey and alias for the instance.
        :param teachers: The ids of the teachers allowed to use the plugin instance.
        :param current_mode: Mode of the plugin instance: summarizing, creative or balanced.
        :param total_tokens_spent: The total number of tokens spent.
        :param indexed_chunk_ids:
        :param agent: LLm agent.
        :param conv_time_window: Time window for the conversation in minutes.
        :param policy: List of policies related to the LLMRule instance.
        :param usage: List of usages related to the LLMRule instance.
        :return: created LLMRule instance
        """
        rule = LLMRule(
            document_id=document_id,
            owner=owner,
            apikey_provider=apikey_provider,
            apikey=apikey,
            chosen_key=chosen_key,
            teachers=teachers,
            current_mode=current_mode,
            total_tokens_spent=total_tokens_spent,
            indexed_chunk_ids=indexed_chunk_ids,
            agent=agent,
            conv_time_window=conv_time_window,
        )
        rule.policy.extend(policy)
        rule.usage.extend(usage)
        db.session.add(rule)
        db.session.commit()
        db.session.refresh(rule)
        return rule

    @staticmethod
    def delete_llm_rule(owner_id: int) -> None:
        """
        Deletes the LLM rule of the given owner.
        """
        stmt = delete(LLMRule).where(LLMRule.owner == owner_id)
        db.session.execute(stmt)
        db.session.commit()

    @staticmethod
    def get_llm_rule(owner_id: int) -> LLMRule:
        """
        Gets the LLM rule of the given owner.
        """
        stmt = select(LLMRule).where(LLMRule.owner == owner_id)
        return db.session.scalar(stmt)

    @staticmethod
    def set_policy(
            user: int,
            llm_rule: LLMRule,
            token_time_window_type: str,
            token_time_window_num: int,
            time_window_tokens: int,
            max_tokens: int,
            policy_type: str  # global or student
    ) -> Policy:
        """
        Sets a new policy for the LLM rule. Policy can be a global policy for the whole LLM rule or a student policy
        for the given user.
        """
        policy = Policy(
            for_user=user or None,
            llm_rule_id=llm_rule.id,
            llm_rule=llm_rule,
            token_time_window_type=token_time_window_type,
            token_time_window_num=token_time_window_num,
            time_window_tokens=time_window_tokens,
            max_tokens=max_tokens,
            policy_type=policy_type
        )

        db.session.add(policy)
        db.session.commit()
        db.session.refresh(policy)
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
        stmt = delete(Policy).where(Policy.llm_rule == llm_rule, Policy.for_user.is_(None))
        db.session.execute(stmt)
        db.session.commit()

    @staticmethod
    def delete_student_policy(llm_rule: LLMRule, user_id: int) -> None:
        """
        Deletes the given student policy.
        """
        stmt = delete(Policy).where(Policy.llm_rule == llm_rule, Policy.for_user == user_id)
        db.session.execute(stmt)
        db.session.commit()

    @staticmethod
    def get_global_policy(llm_rule: LLMRule) -> Policy:
        """
        Gets the given global policy.
        """
        stmt = select(Policy).where(Policy.llm_rule == llm_rule, Policy.for_user.is_(None))
        return db.session.scalar(stmt)

    @staticmethod
    def get_student_policy(llm_rule: LLMRule, user_id: int) -> Policy:
        """
        Gets the given user policy.
        """
        stmt = select(Policy).where(Policy.llm_rule == llm_rule, Policy.for_user == user_id)
        return db.session.scalar(stmt)

    @staticmethod
    def set_usage(
            user: int,
            conv_id: int,
            llm_rule: LLMRule,
            used_tokens: int
    ) -> Usage:
        """
        Sets the usage for the given user.
        :param user: ID of the user for the usage.
        :param conv_id: ID of the conversation of the user.
        :param llm_rule: LLM rule instance.
        :param used_tokens: Number of used tokens.
        :return: Usage of the given user.
        """

        usage = Usage(
            user=user,
            conversation_id=conv_id,
            llm_rule_id=llm_rule.id,
            llm_rule=llm_rule,
            used_tokens=used_tokens
        )
        db.session.add(usage)
        db.session.commit()
        db.session.refresh(usage)
        return usage

    @staticmethod
    def delete_usage(user_id: int, llm_rule: LLMRule) -> None:
        """
        Deletes the usage of the given user in the given LLM rule instance.
        """
        stmt = delete(Usage).where(Usage.user == user_id, Usage.llm_rule == llm_rule)
        db.session.execute(stmt)
        db.session.commit()

    @staticmethod
    def get_usage(user_id: int, llm_rule: LLMRule) -> Usage:
        """
        Gets the usage of the given user in the given LLM rule instance.
        """
        stmt = select(Usage).where(Usage.user == user_id, Usage.llm_rule == llm_rule)
        return db.session.scalar(stmt)