{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcTCP.inc                                               }
{   Description:      TCP library conditional defines.                         }
{                                                                              }
{******************************************************************************}

{.DEFINE TLS}

{.DEFINE TCPCLIENT_SOCKS}
{.DEFINE TCPCLIENT_WEBSOCKET}

{.DEFINE DEBUG_TCP}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE DEBUG_TCP}
{$ENDIF}
{$ENDIF}

{$IFDEF DEBUG_TCP}
  {.DEFINE TCP_DEBUG}
  {.DEFINE TCP_DEBUG_DATA}
  {$DEFINE TCP_DEBUG_PROXY}
  {$DEFINE TCP_DEBUG_TLS}
  {.DEFINE TCP_DEBUG_SOCKET}
  {$DEFINE TCP_DEBUG_CONNECTION}
  {.DEFINE TCP_DEBUG_THREAD}
  {.DEFINE TCP_DEBUG_WEBSOCKET}
  {.DEFINE TCP_LOG_PARAMETERS}
  {.DEFINE TCP_LOG_SERVERSTATE}
{$ENDIF}

{$IFDEF TLS}
  {$DEFINE TCP_TLS}
  {$DEFINE TCPCLIENT_TLS}
  {$DEFINE TCPSERVER_TLS}
{$ENDIF}

