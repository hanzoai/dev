from typing import TYPE_CHECKING

import ide.agenthub.loc_agent.function_calling as locagent_function_calling
from ide.agenthub.codeact_agent import CodeActAgent
from ide.core.config import AgentConfig
from ide.core.logger import ide_logger as logger
from ide.llm.llm import LLM

if TYPE_CHECKING:
    from ide.events.action import Action
    from ide.llm.llm import ModelResponse


class LocAgent(CodeActAgent):
    VERSION = '1.0'

    def __init__(
        self,
        llm: LLM,
        config: AgentConfig,
    ) -> None:
        """Initializes a new instance of the LocAgent class.

        Parameters:
        - llm (LLM): The llm to be used by this agent
        - config (AgentConfig): The configuration for the agent
        """
        super().__init__(llm, config)

        self.tools = locagent_function_calling.get_tools()
        logger.debug(
            f'TOOLS loaded for LocAgent: {", ".join([tool.get("function").get("name") for tool in self.tools])}'
        )

    def response_to_actions(self, response: 'ModelResponse') -> list['Action']:
        return locagent_function_calling.response_to_actions(
            response,
            mcp_tool_names=list(self.mcp_tools.keys()),
        )
