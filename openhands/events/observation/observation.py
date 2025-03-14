from dataclasses import dataclass

from hanzo.events.event import Event


@dataclass
class Observation(Event):
    content: str
