from ide.agenthub.visualbrowsing_agent.visualbrowsing_agent import (
    VisualBrowsingAgent,
)
from ide.controller.agent import Agent

Agent.register('VisualBrowsingAgent', VisualBrowsingAgent)
