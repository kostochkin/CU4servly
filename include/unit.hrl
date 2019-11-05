-record(unit_type, {t :: temperature | sspd | unknown, m :: undefined | integer()}). 
-record(unit, {interface :: rs485, address :: integer(), type :: undefined | #unit_type{}}).

-define(UNIT_TYPES, [{<<"TD">>, temperature}, {<<"SD">>, sspd}]).

