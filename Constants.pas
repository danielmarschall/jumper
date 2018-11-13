unit Constants;

interface

uses
  SysUtils;

const
  // Metrik
  MET_FIELD_SPACE = 3;
  MET_FIELD_SIZE = 37;
  MET_HALFTAB_SIZE = (MET_FIELD_SIZE+MET_FIELD_SPACE) div 2; // 20
  MET_PREVIEW_SIZE_RATIO = 0.30;
  MET_PERCENT_PNL_TIME = 0.28;
  MET_PERCENT_PNL_STONES = 0.47;
  MET_OUTER_MARGIN = 8;
  MET_SHAPE_MARGIN = 3;

  // Resourcennamen
  RES_JUMP = 'Jump';
  RES_UNDO = 'Undo';
  RES_WIN1 = 'Win1';
  RES_WIN2 = 'Win2';
  RES_LOSE = 'Lose';
  RES_EMPTY = 'EmptyField';
  RES_GREEN = 'GreenStone';
  RES_YELLOW = 'YellowStone';
  RES_RED = 'RedStone';

  // Registry
  REG_KEY = 'Software\ViaThinkSoft\PegSolitaire\';
  REG_SOUND = 'Sound';
  REG_PLAYERNAME = 'LastPlayerName';
  REG_REPLAY = 'Replay';

  // Levels
  LVL_EXT = '.brd';
  LVL_PATH = 'Boards' + PathDelim;
  LVL_FILE = LVL_PATH + '%s' + LVL_EXT;

  // Journal
  JNL_EXT = '.jnl';
  JNL_PATH = 'Journal' + PathDelim;
  JNL_FILE = JNL_PATH + '%s' + JNL_EXT;
  JNL_SEP = '|';
  JNL_ENTRY = '%s' + JNL_SEP + '%s' + JNL_SEP + '%d' + JNL_SEP + '%d' + JNL_SEP + '%d' + JNL_SEP + '%d';

resourcestring
  LNG_POINTS = 'Score: %d';
  LNG_TIME = 'Time: %s';
  LNG_COULD_NOT_CREATE_DIR = 'Warning: Could not create directory "%s".';

implementation

end.
