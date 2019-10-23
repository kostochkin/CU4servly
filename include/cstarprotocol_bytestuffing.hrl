-define(SubstShift, 3).
-define(StartByte, 16#F9).
-define(StopByte, 16#FA).
-define(EscapeByte, 16#FB).
-define(StartSubstByte, (?StartByte + ?SubstShift)).
-define(StopSubstByte, (?StopByte + ?SubstShift)).
-define(EscapeSubstByte, (?EscapeByte + ?SubstShift)).

