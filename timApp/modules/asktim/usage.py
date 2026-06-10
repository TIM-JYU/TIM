from __future__ import annotations
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from timApp.modules.asktim.llm_rule import LLMRule
from sqlalchemy import Integer, ForeignKey, JSON, select, delete
from sqlalchemy.ext.mutable import MutableList
from sqlalchemy.orm import Mapped, mapped_column, relationship
from timApp.timdb.sqa import db


class Usage(db.Model):
    """Associates a user and a conversation with an LLMRule context and tracks token usage."""

    __tablename__ = "usage"

    id: Mapped[int] = mapped_column(primary_key=True)
    user: Mapped[int] = mapped_column(ForeignKey("useraccount.id"))
    llm_rule_id: Mapped[int] = mapped_column(
        ForeignKey("llm_rule.id", ondelete="CASCADE")
    )
    llm_rule: Mapped[LLMRule] = relationship("LLMRule", back_populates="usage")
    used_tokens: Mapped[int] = mapped_column(Integer)
    token_usage_history: Mapped[list[dict[str, int]]] = mapped_column(
        MutableList.as_mutable(JSON()),
        default=list,
    )  # [{"timestamp": 123, "tokens": 456}]

    @staticmethod
    def set_usage(
        user: int,
        llm_rule: LLMRule,
        used_tokens: int,
        token_usage_history: list[dict[str, int]] | None = None,
    ) -> Usage:
        """
        Sets the usage for the given user in the given LLM rule context.
        Values overwrite the existing ones.
        If usage does not exist for this user, it is created with used_tokens.
        :param user: ID of the user for the usage.
        :param llm_rule: LLM rule instance.
        :param used_tokens: Number of used tokens.
        :param token_usage_history: Token usage history. Example: [{"timestamp": 123, "tokens": 456}]
        :return: Usage of the given user.
        """
        if token_usage_history is None:
            token_usage_history = []

        usage = Usage.get_usage(llm_rule, user)
        if not usage:
            usage = Usage(
                user=user,
                llm_rule_id=llm_rule.id,
                llm_rule=llm_rule,
                used_tokens=used_tokens,
                token_usage_history=token_usage_history,
            )
            db.session.add(usage)
        else:
            usage.used_tokens = used_tokens
            usage.token_usage_history = token_usage_history

        db.session.commit()
        return usage

    @staticmethod
    def update_usage(
        user: int,
        llm_rule: LLMRule,
        used_tokens: int,
        timestamp: int,
    ) -> Usage:
        """
        Updates the usage for the given user in the given LLM rule context.
        Given used tokens are added to total usage and usage added automatically with
        timestamp to usage history. New Usage is created if one does not exist.
        :param user: ID of the user for the usage.
        :param llm_rule: LLM rule instance.
        :param used_tokens: Number of used tokens.
        :param timestamp: Timestamp of the usage.
        :return: Usage of the given user.
        """
        new_history_item = {"timestamp": timestamp, "tokens": used_tokens}
        usage = Usage.get_usage(llm_rule, user)
        if not usage:
            usage = Usage(
                user=user,
                llm_rule_id=llm_rule.id,
                llm_rule=llm_rule,
                used_tokens=used_tokens,
                token_usage_history=[new_history_item],
            )
            db.session.add(usage)
        else:
            usage.used_tokens = usage.used_tokens + used_tokens
            usage.token_usage_history.append(new_history_item)

        db.session.commit()
        return usage

    @staticmethod
    def set_instance_usage(llm_rule: LLMRule, used_tokens: int) -> None:
        """
        Sets the usage in the given LLM rule context.
        Values are overwritten.
        :param llm_rule: LLM rule instance.
        :param used_tokens: Number of used tokens.
        :return: Usage of the given user.
        """
        llm_rule.total_tokens_spent = used_tokens
        db.session.commit()

    @staticmethod
    def update_instance_usage(llm_rule: LLMRule, used_tokens: int) -> None:
        """
        Sets the usage in the given LLM rule context.
        Given used tokens are added to the current usage.
        :param llm_rule: LLM rule instance.
        :param used_tokens: Number of used tokens.
        :return: Usage of the given user.
        """
        llm_rule.total_tokens_spent = llm_rule.total_tokens_spent + used_tokens
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
    def get_usages(llm_rule: LLMRule) -> list[Usage]:
        """
        Gets all usages associated with llm rule.
        :param llm_rule:
        :return:
        """
        stmt = select(Usage).where(
            Usage.llm_rule_id == llm_rule.id,
        )
        return db.session.scalars(stmt).all()

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
