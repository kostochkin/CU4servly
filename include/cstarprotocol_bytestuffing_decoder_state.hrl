-define(DECODER_STATE_SEEK_START, seek_start).
-define(DECODER_STATE_SEEK_STOP, seek_stop).
-define(DECODER_STATE_ESCAPED, escaped).
-define(DECODER_RESULT_PARTIAL, partial_packet).
-record(decoder_state, {state = ?DECODER_STATE_SEEK_START, acc = <<>>, result = <<>>, rest = <<>>}).
