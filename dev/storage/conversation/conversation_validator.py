import os

from dev.utils.import_utils import get_impl


class ConversationValidator:
    """Storage for conversation metadata. May or may not support multiple users depending on the environment."""

    async def validate(self, conversation_id: str, cookies_str: str):
        return None, None


conversation_validator_cls = os.environ.get(
    'DEV_CONVERSATION_VALIDATOR_CLS',
    'dev.storage.conversation.conversation_validator.ConversationValidator',
)
ConversationValidatorImpl = get_impl(ConversationValidator, conversation_validator_cls)
