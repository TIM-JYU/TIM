from sqlalchemy import Integer, String, ForeignKey
from sqlalchemy.orm import Mapped, mapped_column, relationship
from sqlalchemy.dialects.postgresql import ARRAY
from timApp.timdb.sqa import db


class LLMRule(db.Model):
    """Associates an LLM plugin instance with an owner and defines rules for the instance."""
    __tablename__ = "llm_rule"

    id: Mapped[int] = mapped_column(primary_key=True)
    document_id: Mapped[int] = mapped_column(ForeignKey("json.id"))
    owner: Mapped[int] = mapped_column(ForeignKey("user_group.id"))
    apikey_provider: Mapped[str] = mapped_column(String)
    apikey: Mapped[list[str]] = mapped_column(ARRAY(String))  # list of apikeys and publickeys
    chosen_key: Mapped[str] = mapped_column(String)
    teachers: Mapped[list[int]] = mapped_column(ARRAY(Integer))
    current_mode: Mapped[str] = mapped_column(String)  # summarizing or creative
    total_tokens_spent: Mapped[int] = mapped_column(Integer, default=0)
    indexed_chunk_ids: Mapped[list[int]] = mapped_column(ARRAY(Integer))
    agent: Mapped[str] = mapped_column(String)
    conv_time_window: Mapped[int] = mapped_column(Integer)
    policy: Mapped[list["Policy"]] = relationship("Policy", back_populates="llm_rule")
    usage: Mapped[list["Usage"]] = relationship("Usage", back_populates="llm_rule")


class Policy(db.Model):
    """Defines usage limits for the instance."""
    __tablename__ = "policy"

    id: Mapped[int] = mapped_column(primary_key=True)
    for_user: Mapped[int] = mapped_column(ForeignKey("user_group.id"))
    llm_rule_id: Mapped[int] = mapped_column(ForeignKey("llm_rule.id"))
    llm_rule: Mapped["LLMRule"] = relationship("LLMRule", back_populates="policy")
    token_time_window_type: Mapped[str] = mapped_column(String, nullable=False)  # d,h,min,sec
    token_time_window_num: Mapped[int] = mapped_column(Integer, nullable=False)  # 5 of type
    time_window_tokens: Mapped[int] = mapped_column(Integer, nullable=False)  # token limit for the window
    max_tokens: Mapped[int] = mapped_column(Integer)
    policy_type: Mapped[str] = mapped_column(String)  # global or user


class Usage(db.Model):
    """Associates a user and a conversation with an LLMRule context and tracks token usage."""
    __tablename__ = "usage"

    id: Mapped[int] = mapped_column(primary_key=True)
    user: Mapped[int] = mapped_column(ForeignKey("user_group.id"))
    conversation_id: Mapped[int] = mapped_column(Integer)
    llm_rule_id: Mapped[int] = mapped_column(ForeignKey("llm_rule.id"))
    llm_rule: Mapped[LLMRule] = relationship("LLMRule", back_populates="usage")
    used_tokens: Mapped[int] = mapped_column(Integer)