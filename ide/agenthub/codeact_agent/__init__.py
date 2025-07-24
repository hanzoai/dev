from ide.agenthub.codeact_agent.codeact_agent import CodeActAgent
from ide.controller.agent import Agent

Agent.register('CodeActAgent', CodeActAgent)
