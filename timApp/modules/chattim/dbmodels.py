from sqlalchemy import Integer, String, ForeignKey
from sqlalchemy import Mapped, mapped_column
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup


class LLMRule(db.Model):
    """Associates an LLM plugin instance with an owner and defines rules for the instance."""
    __tablename__ = "llm_rule"

    id: Mapped[int] = mapped_column(primary_key=True)
    document_id: Mapped[int] = mapped_column(ForeignKey("json.id"))
    owner: Mapped[UserGroup] = mapped_column(ForeignKey("user_group.id"))
    apikey: Mapped[str] = mapped_column(String)
    publickey: Mapped[str] = mapped_column(String)
    teachers: Mapped[list[UserGroup]] = mapped_column(ForeignKey("user_group.id"))
    current_mode: Mapped[str] = mapped_column(String, default="summary") # freeform or summary
    total_tokens_spent: Mapped[int] = mapped_column(Integer, default=0)
    indexed_chunk_ids: Mapped[int] = mapped_column(Integer)
    agent: Mapped[str] = mapped_column(String)

    conv_time_window: Mapped[int] = mapped_column(Integer)


class Policy(db.Model):
    """Defines usage limits for the instance."""
    __tablename__ = "policy"

    for_user: Mapped[UserGroup] = mapped_column(ForeignKey("user_group.id"))
    llm_rule: Mapped[LLMRule] = mapped_column(ForeignKey("llm_rule.id"))
    token_time_window_type: Mapped[str] = mapped_column(String, nullable=False) # 'd','h','min','sec'
    token_time_window_num: Mapped[int] = mapped_column(Integer, nullable=False) # 5 of type
    time_window_tokens: Mapped[int] = mapped_column(Integer, nullable=False) # token limit for the window
    max_tokens: Mapped[int] = mapped_column(Integer)
    policy_type: Mapped[str] = mapped_column(String, default="global") # global or user


class Usage(db.Model):
    """Associates a user and a conversation with an LLMRule context and tracks token usage."""
    __tablename__ = "usage"

    user: Mapped[UserGroup] = mapped_column(ForeignKey("user_group.id"))
    conversation_id: Mapped[int] = mapped_column(Integer)
    context: Mapped[LLMRule] = mapped_column(ForeignKey("llm_rule.id"))
    used_tokens: Mapped[int] = mapped_column(Integer)