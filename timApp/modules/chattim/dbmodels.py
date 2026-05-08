from typing import Optional, List
from sqlalchemy import Integer, String, ForeignKey
from sqlalchemy.orm import Mapped, mapped_column, relationship
from sqlalchemy.dialects.postgresql import ARRAY
from timApp.timdb.sqa import db
from timApp.document.docentry import DocEntry
from timApp.user.usergroup import UserGroup


class LLMRule(db.Model):
    """Associates an LLM plugin instance with an owner and defines rules for the instance."""

    __tablename__ = "llm_rule"

    id: Mapped[int] = mapped_column(primary_key=True)
    document_id: Mapped[int] = mapped_column(Integer, default=0)
    owner: Mapped[int] = mapped_column(ForeignKey("useraccount.id"))

    # API-key fields
    public_key: Mapped[str] = mapped_column(String, default="")
    """The public key used associated with the API key or the chosen key for the plugin."""
    provider: Mapped[str] = mapped_column(String, default="")
    """Provider of the API key. Only used when saving the key."""
    api_key: Mapped[str] = mapped_column(String, default="")
    """The API key. Only used when saving the key."""
    groups: Mapped[list[int]] = mapped_column(ARRAY(Integer), default=[])
    """User group IDs that have access to this API-key using `public_key`."""
    paths: Mapped[list[str]] = mapped_column(ARRAY(String), default=[])
    """Document or folder paths where the API key can be used on."""

    teachers: Mapped[list[int]] = mapped_column(ARRAY(Integer), default=[])
    current_mode: Mapped[str] = mapped_column(
        String, default=""
    )  # summarizing or creative
    total_tokens_spent: Mapped[int] = mapped_column(Integer, default=0)
    indexed_document_ids: Mapped[list[int]] = mapped_column(ARRAY(Integer), default=[])
    system_prompt_path: Mapped[str] = mapped_column(String, default="")
    # TODO: this should maybe be something like this
    # system_prompt_doc: Mapped["DocEntry"] = relationship(back_populates="llm_rule")

    agent: Mapped[str] = mapped_column(String, default="")
    conv_time_window: Mapped[int] = mapped_column(Integer, default=0)
    policy: Mapped[list["Policy"]] = relationship(
        "Policy", back_populates="llm_rule", cascade="all, delete-orphan"
    )
    usage: Mapped[list["Usage"]] = relationship(
        "Usage", back_populates="llm_rule", cascade="all, delete-orphan"
    )


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
    token_time_window_type: Mapped[str] = mapped_column(
        String, nullable=False
    )  # d,h,min,sec
    token_time_window_num: Mapped[int] = mapped_column(
        Integer, nullable=False
    )  # 5 of type
    time_window_tokens: Mapped[int] = mapped_column(
        Integer, nullable=False
    )  # token limit for the window
    max_tokens_per_user: Mapped[int] = mapped_column(Integer)
    token_pool: Mapped[int] = mapped_column(Integer)
    policy_type: Mapped[str] = mapped_column(String)  # global or user


class Usage(db.Model):
    """Associates a user and a conversation with an LLMRule context and tracks token usage."""

    __tablename__ = "usage"

    id: Mapped[int] = mapped_column(primary_key=True)
    user: Mapped[int] = mapped_column(ForeignKey("useraccount.id"))
    conversation_id: Mapped[int] = mapped_column(Integer)
    llm_rule_id: Mapped[int] = mapped_column(
        ForeignKey("llm_rule.id", ondelete="CASCADE")
    )
    llm_rule: Mapped[LLMRule] = relationship("LLMRule", back_populates="usage")
    used_tokens: Mapped[int] = mapped_column(Integer)
