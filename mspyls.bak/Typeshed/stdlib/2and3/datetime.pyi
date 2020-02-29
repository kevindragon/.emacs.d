import sys
from time import struct_time
from typing import (
    AnyStr, Optional, SupportsAbs, Tuple, Union, overload,
    ClassVar,
)

if sys.version_info >= (3,):
    _Text = str
else:
    _Text = Union[str, unicode]

MINYEAR: int
MAXYEAR: int

class tzinfo:
    def tzname(self, dt: Optional[datetime]) -> Optional[str]: ...
    def utcoffset(self, dt: Optional[datetime]) -> Optional[timedelta]: ...
    def dst(self, dt: Optional[datetime]) -> Optional[timedelta]: ...
    def fromutc(self, dt: datetime) -> datetime: ...

if sys.version_info >= (3, 2):
    class timezone(tzinfo):
        utc: ClassVar[timezone]
        min: ClassVar[timezone]
        max: ClassVar[timezone]

        def __init__(self, offset: timedelta, name: str = ...) -> None: ...
        def __hash__(self) -> int: ...

_tzinfo = tzinfo

class date:
    min: ClassVar[date]
    max: ClassVar[date]
    resolution: ClassVar[timedelta]

    def __init__(self, year: int, month: int, day: int) -> None: ...

    @classmethod
    def fromtimestamp(cls, t: float) -> date: ...
    @classmethod
    def today(cls) -> date: ...
    @classmethod
    def fromordinal(cls, n: int) -> date: ...

    @property
    def year(self) -> int: ...
    @property
    def month(self) -> int: ...
    @property
    def day(self) -> int: ...

    def ctime(self) -> str: ...
    def strftime(self, fmt: _Text) -> str: ...
    if sys.version_info >= (3,):
        def __format__(self, fmt: str) -> str: ...
    else:
        def __format__(self, fmt: AnyStr) -> AnyStr: ...
    def isoformat(self) -> str: ...
    def timetuple(self) -> struct_time: ...
    def toordinal(self) -> int: ...
    def replace(self, year: int = ..., month: int = ..., day: int = ...) -> date: ...
    def __le__(self, other: date) -> bool: ...
    def __lt__(self, other: date) -> bool: ...
    def __ge__(self, other: date) -> bool: ...
    def __gt__(self, other: date) -> bool: ...
    def __add__(self, other: timedelta) -> date: ...
    @overload
    def __sub__(self, other: timedelta) -> date: ...
    @overload
    def __sub__(self, other: date) -> timedelta: ...
    def __hash__(self) -> int: ...
    def weekday(self) -> int: ...
    def isoweekday(self) -> int: ...
    def isocalendar(self) -> Tuple[int, int, int]: ...

class time:
    min: ClassVar[time]
    max: ClassVar[time]
    resolution: ClassVar[timedelta]

    def __init__(self, hour: int = ..., minute: int = ..., second: int = ..., microsecond: int = ...,
                 tzinfo: Optional[tzinfo] = ...) -> None: ...

    @property
    def hour(self) -> int: ...
    @property
    def minute(self) -> int: ...
    @property
    def second(self) -> int: ...
    @property
    def microsecond(self) -> int: ...
    @property
    def tzinfo(self) -> Optional[_tzinfo]: ...
    if sys.version_info >= (3, 6):
        @property
        def fold(self) -> int: ...

    def __le__(self, other: time) -> bool: ...
    def __lt__(self, other: time) -> bool: ...
    def __ge__(self, other: time) -> bool: ...
    def __gt__(self, other: time) -> bool: ...
    def __hash__(self) -> int: ...
    def isoformat(self) -> str: ...
    def strftime(self, fmt: _Text) -> str: ...
    if sys.version_info >= (3,):
        def __format__(self, fmt: str) -> str: ...
    else:
        def __format__(self, fmt: AnyStr) -> AnyStr: ...
    def utcoffset(self) -> Optional[timedelta]: ...
    def tzname(self) -> Optional[str]: ...
    def dst(self) -> Optional[int]: ...
    if sys.version_info >= (3, 6):
        def replace(self, hour: int = ..., minute: int = ..., second: int = ...,
                    microsecond: int = ..., tzinfo: Optional[_tzinfo] = ...,
                    *, fold: int = ...) -> time: ...
    else:
        def replace(self, hour: int = ..., minute: int = ..., second: int = ...,
                    microsecond: int = ..., tzinfo: Optional[_tzinfo] = ...) -> time: ...

_date = date
_time = time

class timedelta(SupportsAbs[timedelta]):
    min: ClassVar[timedelta]
    max: ClassVar[timedelta]
    resolution: ClassVar[timedelta]

    def __init__(self, days: float = ..., seconds: float = ..., microseconds: float = ...,
                 milliseconds: float = ..., minutes: float = ..., hours: float = ...,
                 weeks: float = ...) -> None: ...

    @property
    def days(self) -> int: ...
    @property
    def seconds(self) -> int: ...
    @property
    def microseconds(self) -> int: ...

    def total_seconds(self) -> float: ...
    def __add__(self, other: timedelta) -> timedelta: ...
    def __radd__(self, other: timedelta) -> timedelta: ...
    def __sub__(self, other: timedelta) -> timedelta: ...
    def __rsub__(self, other: timedelta) -> timedelta: ...
    def __neg__(self) -> timedelta: ...
    def __pos__(self) -> timedelta: ...
    def __abs__(self) -> timedelta: ...
    def __mul__(self, other: float) -> timedelta: ...
    def __rmul__(self, other: float) -> timedelta: ...
    @overload
    def __floordiv__(self, other: timedelta) -> int: ...
    @overload
    def __floordiv__(self, other: int) -> timedelta: ...
    if sys.version_info >= (3,):
        @overload
        def __truediv__(self, other: timedelta) -> float: ...
        @overload
        def __truediv__(self, other: float) -> timedelta: ...
        def __mod__(self, other: timedelta) -> timedelta: ...
        def __divmod__(self, other: timedelta) -> Tuple[int, timedelta]: ...
    else:
        @overload
        def __div__(self, other: timedelta) -> float: ...
        @overload
        def __div__(self, other: float) -> timedelta: ...
    def __le__(self, other: timedelta) -> bool: ...
    def __lt__(self, other: timedelta) -> bool: ...
    def __ge__(self, other: timedelta) -> bool: ...
    def __gt__(self, other: timedelta) -> bool: ...
    def __hash__(self) -> int: ...

class datetime:
    # TODO: Is a subclass of date, but this would make some types incompatible.
    min: ClassVar[datetime]
    max: ClassVar[datetime]
    resolution: ClassVar[timedelta]

    if sys.version_info >= (3, 6):
        def __init__(self, year: int, month: int, day: int, hour: int = ...,
                     minute: int = ..., second: int = ..., microsecond: int = ...,
                     tzinfo: Optional[tzinfo] = ..., *, fold: int = ...) -> None: ...
    else:
        def __init__(self, year: int, month: int, day: int, hour: int = ...,
                     minute: int = ..., second: int = ..., microsecond: int = ...,
                     tzinfo: Optional[tzinfo] = ...) -> None: ...

    @property
    def year(self) -> int: ...
    @property
    def month(self) -> int: ...
    @property
    def day(self) -> int: ...
    @property
    def hour(self) -> int: ...
    @property
    def minute(self) -> int: ...
    @property
    def second(self) -> int: ...
    @property
    def microsecond(self) -> int: ...
    @property
    def tzinfo(self) -> Optional[_tzinfo]: ...
    if sys.version_info >= (3, 6):
        @property
        def fold(self) -> int: ...

    @classmethod
    def fromtimestamp(cls, t: float, tz: Optional[_tzinfo] = ...) -> datetime: ...
    @classmethod
    def utcfromtimestamp(cls, t: float) -> datetime: ...
    @classmethod
    def today(cls) -> datetime: ...
    @classmethod
    def fromordinal(cls, n: int) -> datetime: ...
    @classmethod
    def now(cls, tz: Optional[_tzinfo] = ...) -> datetime: ...
    @classmethod
    def utcnow(cls) -> datetime: ...
    if sys.version_info >= (3, 6):
        @classmethod
        def combine(cls, date: date, time: time, tzinfo: Optional[_tzinfo] = ...) -> datetime: ...
    else:
        @classmethod
        def combine(cls, date: date, time: time) -> datetime: ...
    if sys.version_info >= (3, 7):
        @classmethod
        def fromisoformat(cls, date_string: str) -> datetime: ...
    def strftime(self, fmt: _Text) -> str: ...
    if sys.version_info >= (3,):
        def __format__(self, fmt: str) -> str: ...
    else:
        def __format__(self, fmt: AnyStr) -> AnyStr: ...
    def toordinal(self) -> int: ...
    def timetuple(self) -> struct_time: ...
    if sys.version_info >= (3, 3):
        def timestamp(self) -> float: ...
    def utctimetuple(self) -> struct_time: ...
    def date(self) -> _date: ...
    def time(self) -> _time: ...
    def timetz(self) -> _time: ...
    if sys.version_info >= (3, 6):
        def replace(self, year: int = ..., month: int = ..., day: int = ..., hour: int = ...,
                    minute: int = ..., second: int = ..., microsecond: int = ..., tzinfo:
                    Optional[_tzinfo] = ..., *, fold: int = ...) -> datetime: ...
    else:
        def replace(self, year: int = ..., month: int = ..., day: int = ..., hour: int = ...,
                    minute: int = ..., second: int = ..., microsecond: int = ..., tzinfo:
                    Optional[_tzinfo] = ...) -> datetime: ...
    if sys.version_info >= (3, 3):
        def astimezone(self, tz: Optional[_tzinfo] = ...) -> datetime: ...
    else:
        def astimezone(self, tz: _tzinfo) -> datetime: ...
    def ctime(self) -> str: ...
    if sys.version_info >= (3, 6):
        def isoformat(self, sep: str = ..., timespec: str = ...) -> str: ...
    else:
        def isoformat(self, sep: str = ...) -> str: ...
    @classmethod
    def strptime(cls, date_string: _Text, format: _Text) -> datetime: ...
    def utcoffset(self) -> Optional[timedelta]: ...
    def tzname(self) -> Optional[str]: ...
    def dst(self) -> Optional[int]: ...
    def __le__(self, other: datetime) -> bool: ...
    def __lt__(self, other: datetime) -> bool: ...
    def __ge__(self, other: datetime) -> bool: ...
    def __gt__(self, other: datetime) -> bool: ...
    def __add__(self, other: timedelta) -> datetime: ...
    @overload
    def __sub__(self, other: datetime) -> timedelta: ...
    @overload
    def __sub__(self, other: timedelta) -> datetime: ...
    def __hash__(self) -> int: ...
    def weekday(self) -> int: ...
    def isoweekday(self) -> int: ...
    def isocalendar(self) -> Tuple[int, int, int]: ...
