from __future__ import annotations
from typing import Optional
from sqlalchemy import Integer, String, ForeignKey, select, delete
from sqlalchemy.orm import Mapped, mapped_column, relationship
from timApp.timdb.sqa import db
from timApp.modules.asktim.llm_rule import LLMRule


class Policy(db.Model):
    """Defines usage limits for the instance."""

    __tablename__ = "policy"

    id: Mapped[int] = mapped_column(primary_key=True)
    for_user: Mapped[Optional[int]] = mapped_column(
        ForeignKey("useraccount.id"), nullable=True
    )
    llm_rule_id: Mapped[int] = mapped_column(
        ForeignKey("llm_rule.id", ondelete="CASCADE")
    )
    llm_rule: Mapped["LLMRule"] = relationship("LLMRule", back_populates="policy")
    token_time_window_type: Mapped[Optional[str]] = mapped_column(
        String, nullable=True
    )  # d,h,min,sec
    token_time_window_num: Mapped[Optional[int]] = mapped_column(
        Integer, nullable=True
    )  # 5 of type
    time_window_tokens: Mapped[Optional[int]] = mapped_column(
        Integer, nullable=True
    )  # token limit for the window
    max_tokens_per_user: Mapped[Optional[int]] = mapped_column(Integer, nullable=True)
    token_pool: Mapped[Optional[int]] = mapped_column(Integer, nullable=True)

    @staticmethod
    def set_policy(
        user: int,
        llm_rule: LLMRule,
        token_time_window_type: str | None,
        token_time_window_num: int | None,
        time_window_tokens: int | None,
        max_tokens_per_user: int | None,
        token_pool: int | None,
    ) -> Policy:
        """
        Sets a new policy for the LLM rule. Policy can be a global policy for the whole LLM rule or a student policy
        for the given user.
        """
        if user:
            policy = Policy.get_user_policy(llm_rule, user)
        else:
            policy = Policy.get_global_policy(llm_rule)
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
            )
            db.session.add(policy)
        else:
            policy.token_time_window_type = token_time_window_type
            policy.token_time_window_num = token_time_window_num
            policy.time_window_tokens = time_window_tokens
            policy.max_tokens_per_user = max_tokens_per_user
            policy.token_pool = token_pool
        db.session.commit()
        return policy

    @staticmethod
    def set_global_policy(
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
        policy = Policy.set_policy(
            0,
            llm_rule,
            token_time_window_type,
            token_time_window_num,
            time_window_tokens,
            max_tokens_per_user,
            token_pool,
        )
        return policy

    @staticmethod
    def set_user_policy(
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
        policy = Policy.set_policy(
            user,
            llm_rule,
            token_time_window_type,
            token_time_window_num,
            time_window_tokens,
            max_tokens_per_user,
            0,
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
            Policy.for_user == 0,
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
            Policy.for_user == 0,
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
