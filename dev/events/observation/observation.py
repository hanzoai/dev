from dataclasses import dataclass

from dev.events.event import Event


@dataclass
class Observation(Event):
    content: str
