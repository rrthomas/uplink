fixed1 m = 1.5f;
fixed5 n = 10.00000f;
float w = 4.5;
int x = 5;
int y = 2;
int z = 3;
bool t = True;
bool f = True;

assetBin a = 'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65';
account b = 'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65';

timedelta td = 1y1mo1d1h1m1s;
datetime dt_future;
datetime dt_past;

transition initial -> terminal;

/* test basic bin ops on TCrypto/TFloat/TInt/TFixed/TDateTime/TDelta types */
/* TODO: Write comments above each test describing the binop tested */
@initial
f () {
  dt_ = now();
  dt_future = "2090-10-10T23:13:40+05:00";
  dt_past = "1999-02-23T23:13:40+05:00";
  dt_past_plus1 = dt_past + td;              /* add delta to datetimes */
  delta3 = (td * 2) + td;                    /* add and mult delta */
  dt_past = dt_past_plus1 + delta3;

  c = 1;
  d = y * c;
  e = 2.0 * w;
  w = e - w + 1.5;
  x = 2*x - x;    
  z = d + y;     
  y = y - z;
  y = x / y;
  w = w / 2.0;

  m = m * 2.0f / 3.0f - 5.0f;
  n = -55.00000f + n;

  if (t || f) {
    t = False;
  } else {
    f = True;
  };

  before (dt_future) {
    x = x + 1;
  };
  before (dt_past) {
    y = y + 10000;
  };

  after (dt_future) {
    x = x + 10000;
  };
  after (dt_past) {
    y = y + 1;
  };

  between (dt_past, dt_future) {
    x = x * 3;
  };

  terminate("bye");
}
