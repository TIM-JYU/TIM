from __future__ import annotations
from typing import Optional
from sqlalchemy import Integer, String, ForeignKey
from sqlalchemy.orm import Mapped, mapped_column, relationship
from sqlalchemy.dialects.postgresql import ARRAY
from timApp.timdb.sqa import db
from timApp.modules.asktim.policy import Policy
from timApp.modules.asktim.usage import Usage
from timApp.user.user import User
from sqlalchemy import select, delete
from timApp.user.usergroup import UserGroup


class LLMRule(db.Model):
    """Associates an LLM plugin instance with an owner and defines rules for the instance."""

    __tablename__ = "llm_rule"

    id: Mapped[int] = mapped_column(primary_key=True)
    document_id: Mapped[int] = mapped_column(Integer, default=0)
    owner: Mapped[int] = mapped_column(ForeignKey("useraccount.id"))

    # API key fields
    public_key: Mapped[str] = mapped_column(String, default="")
    """The public key associated with the API key or the chosen key for the plugin."""
    provider: Mapped[str] = mapped_column(String, default="")
    """Provider of the API key. Only used when saving the key."""
    api_key: Mapped[str] = mapped_column(String, default="")
    """The API key. Only used when saving the key."""
    groups: Mapped[list[int]] = mapped_column(ARRAY(Integer), default=[])
    """User group IDs that have access to this API-key using `public_key`."""
    paths: Mapped[list[str]] = mapped_column(ARRAY(String), default=[])
    """Document or folder paths where the API key can be used on."""

    # Model options
    use_streaming: Mapped[bool] = mapped_column(default=False)
    """If `True`, the plugin uses streaming model response."""
    temperature: Mapped[Optional[float]] = mapped_column(nullable=True)
    """
    Optional temperature setting to use when creating the model response.
    If null, the temperature parameter is not used.
    """
    include_citations: Mapped[bool] = mapped_column(default=False)
    """Whether to include citations to the agent messages."""
    similarity_threshold: Mapped[Optional[float]] = mapped_column(nullable=True)
    """Threshold for a block to be included in the agent context. Should be between -1 and 1."""
    top_k_chunks: Mapped[int] = mapped_column(Integer, default=3)
    """Number of most relevant chunks to retrieve for context."""

    current_mode: Mapped[str] = mapped_column(
        String, default=""
    )  # summarizing or creative
    total_tokens_spent: Mapped[int] = mapped_column(Integer, default=0)
    indexed_document_ids: Mapped[list[int]] = mapped_column(ARRAY(Integer), default=[])
    system_prompt_path: Mapped[str] = mapped_column(String, default="")

    agent: Mapped[str] = mapped_column(String, default="")
    conv_time_window: Mapped[int] = mapped_column(Integer, default=0)
    """Time window for messages to be considered in the conversation context."""
    conv_messages_max: Mapped[int] = mapped_column(Integer, default=32)
    """Maximum amount of messages in the conversation."""
    policy: Mapped[list["Policy"]] = relationship(
        "Policy", back_populates="llm_rule", cascade="all, delete-orphan"
    )
    usage: Mapped[list["Usage"]] = relationship(
        "Usage", back_populates="llm_rule", cascade="all, delete-orphan"
    )

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
        current_mode: str,
        total_tokens_spent: int,
        indexed_document_ids: list[int],
        system_prompt_path: str,
        agent: str,
        conv_time_window: int,
        conv_messages_max: int,
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
        :param current_mode: Mode of the plugin instance: summarizing, creative or balanced.
        :param total_tokens_spent: The total number of tokens spent.
        :param indexed_document_ids:
        :param system_prompt_path: TIM path for an optional custom system prompt.
        :param agent: LLm agent.
        :param conv_time_window: Time window for the conversation in seconds.
        :param conv_messages_max: Maximum amount of messages in the conversation.
        :param policy: List of policies related to the LLMRule instance.
        :param usage: List of usages related to the LLMRule instance.
        :return: created LLMRule instance
        """
        rule = LLMRule.get_llm_rule(document_id)
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
                current_mode=current_mode,
                total_tokens_spent=total_tokens_spent,
                indexed_document_ids=indexed_document_ids,
                system_prompt_path=system_prompt_path,
                agent=agent,
                conv_time_window=conv_time_window,
                conv_messages_max=conv_messages_max,
            )
            db.session.add(rule)
        else:
            rule.public_key = public_key
            rule.use_streaming = use_streaming
            rule.temperature = temperature
            rule.include_citations = include_citations
            rule.similarity_threshold = similarity_threshold
            rule.top_k_chunks = top_k_chunks
            rule.current_mode = current_mode
            rule.total_tokens_spent = total_tokens_spent
            rule.indexed_document_ids = indexed_document_ids
            rule.system_prompt_path = system_prompt_path
            rule.agent = agent
            rule.conv_time_window = conv_time_window
            rule.conv_messages_max = conv_messages_max
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
        rule = LLMRule.get_api_key_by_alias(public_key)
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
    def access_api_key(user_id: int, public_key: str) -> LLMRule | None:
        """
        Get the API key if the given user has access to it.
        User has access to the key if it's the owner of the key, or
        it is included in some of the user groups that have access.

        :param user_id: The id of the user.
        :param public_key: The associated public key for the desired API key.
        :return: The API key or None if it does not exist or the user has no access.
        """
        user = User.get_by_id(user_id)
        if not user:
            return None

        api_key = LLMRule.get_api_key_by_alias(public_key)
        if not api_key:
            return None
        if user_id == api_key.owner:
            return api_key
        group_ids: list[int] = api_key.groups or []

        if not group_ids:
            return None

        user_group_ids = {g.id for g in user.groups}
        if user_group_ids & set(group_ids):
            return api_key

        return None

    @staticmethod
    def update_api_key_permissions(
        owner_id: int, alias: str, groups: list[str], paths: list[str]
    ) -> LLMRule:
        """Update the API key permissions."""
        rule = LLMRule.get_owner_api_key(owner_id, alias)
        if not rule:
            raise Exception("No API-key with the alias found")

        user = User.get_by_id(owner_id)
        if not user:
            raise Exception(f"Invalid user.")

        user_groups = LLMRule.get_user_groups(groups)
        LLMRule.validate_item_paths(user, paths)

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
        rule = LLMRule.get_owner_api_key(owner_id, public_key)
        if not rule:
            raise Exception("No API-key with the alias found")
        groups = rule.groups
        rule.groups = filter(lambda gid: gid != group_id, groups)  # type: ignore
        db.session.commit()

    @staticmethod
    def delete_api_key(owner_id: int, alias: str) -> None:
        """Deletes the LLM rule row of the owner based on the alias."""
        key = LLMRule.get_owner_api_key(owner_id, alias)
        if not key:
            raise Exception("No API key with the alias found.")
        assert isinstance(key, LLMRule)
        db.session.delete(key)
        db.session.commit()

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