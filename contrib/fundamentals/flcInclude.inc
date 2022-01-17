{******************************************************************************}
{                                                                              }
{   File name:        flcInclude.inc                                           }
{   File updated:     2020/07/03                                               }
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   Github:           https://github.com/fundamentalslib                       }
{   E-mail:           fundamentals.library at gmail.com                        }
{                                                                              }
{ Conditionals defined elsewhere:                                              }
{                                                                              }
{   DEBUG             - Enable debug mode.                                     }
{                       Sets compiler options for debug information,           }
{                       range/overflow checking, assertions, etc.              }
{                       If not defined, set compiler options for release.      }
{   PurePascal        - Don't use assembler code.                              }
{                       All code is compiled from pure Pascal for maximum      }
{                       portability.                                           }
{                                                                              }
{ Conditionals defined here:                                                   }
{                                                                              }
{   DELPHI            - Delphi compiler.                                       }
{     DELPHI1         - Delphi 1.                                              }
{     DELPHI2         - Delphi 2.                                              }
{     DELPHI3         - Delphi 3.                                              }
{     DELPHI4         - Delphi 4.                                              }
{     DELPHI5         - Delphi 5.                                              }
{     DELPHI6         - Delphi 6.                                              }
{     DELPHI7         - Delphi 7.                                              }
{     DELPHI8         - Delphi 8.                                              }
{     DELPHI2005      - Delphi 2005.                                           }
{     DELPHI2006      - Delphi 2006.                                           }
{     DELPHI2007      - Delphi 2007.                                           }
{     DELPHI2009      - Delphi 2009.                                           }
{     DELPHI2010      - Delphi 2010.                                           }
{     DELPHIXE        - Delphi XE.                                             }
{     DELPHIXE2       - Delphi XE2.                                            }
{     DELPHIXE3       - Delphi XE3.                                            }
{     DELPHIXE4       - Delphi XE4.                                            }
{     DELPHIXE5       - Delphi XE5.                                            }
{     DELPHIXE6       - Delphi XE6.                                            }
{     DELPHIXE7       - Delphi XE7.                                            }
{     DELPHIXE8       - Delphi XE8.                                            }
{     DELPHI10        - Delphi 10.                                             }
{     DELPHI10_1      - Delphi 10.1.                                           }
{     DELPHI10_2      - Delphi 10.2.                                           }
{     DELPHI10_3      - Delphi 10.3.                                           }
{     DELPHI10_4      - Delphi 10.4.                                           }
{                                                                              }
{     DELPHI2_UP      - Delphi 2 or later.                                     }
{     DELPHI3_UP      - Delphi 3 or later.                                     }
{     DELPHI4_UP      - Delphi 4 or later.                                     }
{     DELPHI5_UP      - Delphi 5 or later.                                     }
{     DELPHI6_UP      - Delphi 6 or later.                                     }
{     DELPHI7_UP      - Delphi 7 or later.                                     }
{     DELPHI8_UP      - Delphi 8 or later.                                     }
{     DELPHI2005_UP   - Delphi 2005 or later.                                  }
{     DELPHI2006_UP   - Delphi 2006 or later.                                  }
{     DELPHI2007_UP   - Delphi 2007 or later.                                  }
{     DELPHI2009_UP   - Delphi 2009 or later.                                  }
{     DELPHI2010_UP   - Delphi 2010 or later.                                  }
{     DELPHIXE_UP     - Delphi XE or later.                                    }
{     DELPHIXE2_UP    - Delphi XE2 or later.                                   }
{     DELPHIXE3_UP    - Delphi XE3 or later.                                   }
{     DELPHIXE4_UP    - Delphi XE4 or later.                                   }
{     DELPHIXE5_UP    - Delphi XE5 or later.                                   }
{     DELPHIXE6_UP    - Delphi XE6 or later.                                   }
{     DELPHIXE7_UP    - Delphi XE7 or later.                                   }
{     DELPHIXE8_UP    - Delphi XE8 or later.                                   }
{     DELPHI10_UP     - Delphi 10 or later.                                    }
{     DELPHI10_1_UP   - Delphi 10.1 or later.                                  }
{     DELPHI10_2_UP   - Delphi 10.2 or later.                                  }
{     DELPHI10_3_UP   - Delphi 10.3 or later.                                  }
{     DELPHI10_4_UP   - Delphi 10.4 or later.                                  }
{                                                                              }
{     DELPHI3_DOWN    - Delphi 3 or earlier.                                   }
{     DELPHI4_DOWN    - Delphi 4 or earlier.                                   }
{     DELPHI5_DOWN    - Delphi 5 or earlier.                                   }
{     DELPHI6_DOWN    - Delphi 6 or earlier.                                   }
{     DELPHI7_DOWN    - Delphi 7 or earlier.                                   }
{                                                                              }
{   KYLIX             - Kylix compiler.                                        }
{   BCB               - Borland C++ Builder compiler.                          }
{                                                                              }
{   FREEPASCAL         - Free Pascal compiler.                                 }
{     FREEPASCAL1      - FreePascal 1.x                                        }
{     FREEPASCAL2      - FreePascal 2.x                                        }
{     FREEPASCAL3      - FreePascal 3.x                                        }
{     FREEPASCAL2_UP   - FreePascal 2.x or later.                              }
{     FREEPASCAL3_UP   - FreePascal 3.x or later.                              }
{     FREEPASCAL2_0    - FreePascal 2.0                                        }
{     FREEPASCAL2_2    - FreePascal 2.2                                        }
{     FREEPASCAL2_4    - FreePascal 2.4                                        }
{     FREEPASCAL2_6    - FreePascal 2.6                                        }
{     FREEPASCAL2_6_UP - FreePascal 2.6 or later.                              }
{     FREEPASCAL2_7    - FreePascal 2.7                                        }
{     FREEPASCAL2_7_UP - FreePascal 2.7 or later.                              }
{     FREEPASCAL3_0    - FreePascal 3.0                                        }
{     FREEPASCAL3_0_UP - FreePascal 3.0 or later.                              }
{     FREEPASCAL3_1    - FreePascal 3.1                                        }
{     FREEPASCAL3_1_UP - FreePascal 3.1 or later.                              }
{     FREEPASCAL3_2    - FreePascal 3.2                                        }
{     FREEPASCAL3_2_UP - FreePascal 3.2 or later.                              }
{                                                                              }
{   SupportCardinal            - Compiler supports Cardinal type.              }
{   SupportDynArray            - Compiler supports dynamic arrays.             }
{   SupportInt64               - Compiler supports Int64 type.                 }
{   SupportUInt64              - Compiler supports UInt64 type.                }
{   SupportNativeInt           - Compiler supports NativeInt type.             }
{   SupportNativeUInt          - Compiler supports NativeUInt type.            }
{   SupportFixedInt            - Compiler supports FixedInt type.              }
{   SupportFixedUInt           - Compiler supports FixedUInt type.             }
{   SupportAnsiChar            - Compiler supports AnsiChar type.              }
{   SupportAnsiString          - Compiler supports AnsiString type.            }
{   SupportWideChar            - Compiler supports WideChar type.              }
{   SupportWideString          - Compiler supports WideString type.            }
{   SupportShortString         - Compiler supports short String type.          }
{   SupportUCS4Char            - Compiler supports UC4Char type.               }
{   SupportUTF8Char            - Compiler supports UTF8Char type.              }
{   SupportUTF8String          - Compiler supports UTF8String type.            }
{   SupportUCS4String          - Cimpiler supports UCS4String type.            }
{   SupportCurrency            - Compiler supports Currency type.              }
{   SupportVariant             - Compiler supports Variant type.               }
{   SupportAssert              - Compiler supports Assert statement.           }
{   SupportInterface           - Compiler supports IInterface.                 }
{   SupportOverload            - Compiler supports method overloading.         }
{   SupportInline              - Compiler supports inline functions.           }
{   SupportForIn               - Compiler supports for-in loop.                }
{   SupportGenerics            - Compiler supports generics.                   }
{   SupportOperatorOverloading - Compiler supports overator overloading.       }
{   SupportAnonymousMethods    - Compiler supports anonymous methods.          }
{   SupportUnicodeString       - Compiler supports UnicodeString type.         }
{   SupportRawByteString       - Compiler supports RawByteString type.         }
{   SupportEnumValue           - Compiler supports enumeration values.         }
{   SupportStringRefCount      - Compiler supports StringRefCount function.    }
{                                                                              }
{   CharIsAnsi          - Default Char type is AnsiChar (1 byte).              }
{   CharIsWide          - Default Char type is WideChar (2 bytes).             }
{   StringIsAnsi        - Default String type is AnsiString.                   }
{   StringIsUnicode     - Default String type is UnicodeString.                }
{   ExtendedIs80Bits    - Extended type has 80 bit (10 byte) precision.        }
{   ExtendedIsDouble    - Extended type is Double.                             }
{   ExtendedIs16Bytes   - Extended type is 16 bytes.                           }
{   TBytesDeclared      - System defines TBytes type.                          }
{   ObjectARC           - Objects has Automatic Reference Countinging (ARC).   }
{   LongWordIs32Bits    - LongWord type is 32 bits.                            }
{   LongIntIs32Bits     - LongInt type is 32 bits.                             }
{   LongWordIs64Bits    - LongWord type is 64 bits.                            }
{   LongIntIs64Bits     - LongInt type is 64 bits.                             }
{   NativeIntIs32Bits   - NativeInt type is 32 bits.                           }
{   NativeIntIs64Bits   - NativeInt type is 64 bits.                           }
{   NativeUIntIs32Bits  - NativeUInt type is 32 bits.                          }
{   NativeUIntIs64Bits  - NativeUInt type is 64 bits.                          }
{                                                                              }
{   UseInline         - SupportInline and not PurePascal.                      }
{                                                                              }
{   OS_POSIX          - Posix type operating system.                           }
{     OS_BSD          - BSD type operating system.                             }
{       OS_FREEBSD    - FreeBSD operating system.                              }
{       OS_NETBSD     - NetBSD operating system.                               }
{       OS_OPENBSD    - OpenBSD operating system.                              }
{     OS_LINUX        - Linux operating system.                                }
{     OS_LINUX32      - Linux 32-bit operating system.                         }
{     OS_LINUX64      - Linux 64-bit operating system.                         }
{     OS_MACOSX       - MacOSX operating system.                               }
{     OS_MACOSX32     - MacOSX 32-bit operating system.                        }
{     OS_MACOSX64     - MacOSX 64-bit operating system.                        }
{     OS_SOLARIS      - Sun Solaris operating system.                          }
{   OS_MOBILE         - Mobile operating system.                               }
{     OS_IOS          - Apple iOS operating system.                            }
{     OS_IOS32        - Apple iOS 32-bit.                                      }
{     OS_IOS64        - Apple iOS 64-bit.                                      }
{     OS_ANDROID      - Android operating system.                              }
{     OS_ANDROID32    - Android 32-bit operating system.                       }
{     OS_ANDROID64    - Android 64-bit operating system.                       }
{   OS_MSWIN          - MS Windows type operating system.                      }
{     OS_WIN16        - Win16 operating system.                                }
{     OS_WIN32        - Win32 operating system.                                }
{     OS_WIN64        - Win64 operating system.                                }
{   OS_OS2            - OS/2 operating system.                                 }
{   OS_DOS            - DOS operating system.                                  }
{     OS_DOS32        - DOS operating system with 32-bit extender.             }
{                                                                              }
{   OS_32             - 32 bit operating system.                               }
{   OS_64             - 64 bit operating system.                               }
{                                                                              }
{   ManagedCode       - Compiles managed code.                                 }
{     DOT_NET         - Compiles .NET code.                                    }
{                                                                              }
{   NativeCode        - Compiles native code.                                  }
{     CPU_386         - Intel 386 32-bit compatible CPU.                       }
{     CPU_X86_64      - x86 64-bit compatible CPU.                             }
{     CPU_68K         - Motorola 68000 compatible CPU.                         }
{     CPU_PPC32       - PowerPC 32-bit CPU.                                    }
{     CPU_PPC64       - PowerPC 64-bit CPU.                                    }
{     CPU_ARM         - ARM based CPU.                                         }
{                                                                              }
{     CPU_32          - 32 bit CPU                                             }
{     CPU_64          - 64 bit CPU                                             }
{                                                                              }
{   WindowsPlatform   - Win32/Win64/.NET code on a Windows platform.           }
{                                                                              }
{   ASM386_DELPHI     - Compiler supports 386 assembler (Delphi style)         }
{   ASM386_FREEPASCAL - Compiler supports 386 assembler (FreePascal style)     }
{   ASM386            - Compiler supports 386 assembler (Generic)              }
{   ASMX86_64         - Compiler supports x86-64 assembler (Generic)           }
{   ASMX86_64_DELPHI  - Compiler supports x86-64 assembler (Delphie style)     }
{                                                                              }
{   IsConsoleApp      - Console application.                                   }
{                                                                              }
{******************************************************************************}



{                                                                              }
{ Delphi 1                                                                     }
{                                                                              }
{$IFDEF VER80}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI1}
  {$DEFINE DELPHI3_DOWN}
  {$DEFINE DELPHI4_DOWN}
  {$DEFINE DELPHI5_DOWN}
  {$DEFINE DELPHI6_DOWN}
  {$DEFINE DELPHI7_DOWN}
{$ENDIF}



{                                                                              }
{ Delphi 2                                                                     }
{                                                                              }
{$IFDEF VER90}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI3_DOWN}
  {$DEFINE DELPHI4_DOWN}
  {$DEFINE DELPHI5_DOWN}
  {$DEFINE DELPHI6_DOWN}
  {$DEFINE DELPHI7_DOWN}
{$ENDIF}
{$IFDEF VER93}
  {$DEFINE BCB}
  {$DEFINE BCB1}
{$ENDIF}



{                                                                              }
{ Delphi 3                                                                     }
{ C Builder 3                                                                  }
{                                                                              }
{$IFDEF VER100}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI3}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI3_DOWN}
  {$DEFINE DELPHI4_DOWN}
  {$DEFINE DELPHI5_DOWN}
  {$DEFINE DELPHI6_DOWN}
  {$DEFINE DELPHI7_DOWN}
{$ENDIF}
{$IFDEF VER110}
  {$DEFINE BCB}
  {$DEFINE BCB3}
{$ENDIF}



{                                                                              }
{ Delphi 4                                                                     }
{                                                                              }
{$IFDEF VER120}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI4}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI4_DOWN}
  {$DEFINE DELPHI5_DOWN}
  {$DEFINE DELPHI6_DOWN}
  {$DEFINE DELPHI7_DOWN}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE BCB}
  {$DEFINE BCB4}
{$ENDIF}



{                                                                              }
{ Delphi 5                                                                     }
{                                                                              }
{$IFDEF VER130}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI5}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI5_DOWN}
  {$DEFINE DELPHI6_DOWN}
  {$DEFINE DELPHI7_DOWN}
{$ENDIF}
{$IFDEF VER135}
  {$DEFINE BCB}
  {$DEFINE BCB5}
{$ENDIF}



{                                                                              }
{ Delphi 6                                                                     }
{                                                                              }
{$IFDEF VER140}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI6_DOWN}
  {$DEFINE DELPHI7_DOWN}
  {$IFDEF LINUX}
    {$DEFINE KYLIX}
  {$ENDIF}
{$ENDIF}



{                                                                              }
{ Delphi 7                                                                     }
{                                                                              }
{$IFDEF VER150}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI7}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI7_DOWN}
  {$IFDEF LINUX}
    {$DEFINE KYLIX}
  {$ENDIF}
{$ENDIF}



{                                                                              }
{ Delphi 8 for .NET                                                            }
{                                                                              }
{$IFDEF VER160}
  {$DEFINE DOT_NET}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI8}
  {$DEFINE DELPHI8_UP}
{$ENDIF}



{                                                                              }
{ Delphi 2005                                                                  }
{                                                                              }
{$IFDEF VER170}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2005}
  {$DEFINE DELPHI2005_UP}
{$ENDIF}



{                                                                              }
{ Delphi 2006                                                                  }
{ Delphi 2006 and Delphi 2007 has VER180 set.                                  }
{ Delphi 2007 has VER180 and VER185 set.                                       }
{                                                                              }
{$IFDEF VER180}
{$IFNDEF VER185}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2006}
  {$DEFINE DELPHI2006_UP}
{$ENDIF}
{$ENDIF}


{                                                                              }
{ Delphi 2007                                                                  }
{                                                                              }
{$IFDEF VER185}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2007}
  {$DEFINE DELPHI2007_UP}
{$ENDIF}



{                                                                              }
{ Delphi 2007 for .NET                                                         }
{                                                                              }
{$IFDEF VER190}
  {$DEFINE DOT_NET}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2007}
  {$DEFINE DELPHI2007_UP}
{$ENDIF}



{                                                                              }
{ Delphi 2009                                                                  }
{                                                                              }
{$IFDEF VER200}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2009}
  {$DEFINE DELPHI2009_UP}
{$ENDIF}



{                                                                              }
{ Delphi 2010                                                                  }
{                                                                              }
{$IFDEF VER210}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2010}
  {$DEFINE DELPHI2010_UP}
{$ENDIF}


{                                                                              }
{ Delphi XE                                                                    }
{                                                                              }
{$IFDEF VER220}
  {$DEFINE DELPHI}
  {$DEFINE DELPHIXE}
  {$DEFINE DELPHIXE_UP}
{$ENDIF}



{                                                                              }
{ Delphi XE2                                                                   }
{                                                                              }
{$IFDEF VER230}
  {$DEFINE DELPHI}
  {$DEFINE DELPHIXE2}
  {$DEFINE DELPHIXE2_UP}
{$ENDIF}



{                                                                              }
{ Delphi XE3                                                                   }
{                                                                              }
{$IFDEF VER240}
  {$DEFINE DELPHI}
  {$DEFINE DELPHIXE3}
  {$DEFINE DELPHIXE3_UP}
{$ENDIF}



{                                                                              }
{ Delphi XE4                                                                   }
{                                                                              }
{$IFDEF VER250}
  {$DEFINE DELPHI}
  {$DEFINE DELPHIXE4}
  {$DEFINE DELPHIXE4_UP}
{$ENDIF}



{                                                                              }
{ Delphi XE5                                                                   }
{                                                                              }
{$IFDEF VER260}
  {$DEFINE DELPHI}
  {$DEFINE DELPHIXE5}
  {$DEFINE DELPHIXE5_UP}
{$ENDIF}



{                                                                              }
{ Delphi XE6                                                                   }
{                                                                              }
{$IFDEF VER270}
  {$DEFINE DELPHI}
  {$DEFINE DELPHIXE6}
  {$DEFINE DELPHIXE6_UP}
{$ENDIF}



{                                                                              }
{ Delphi XE7                                                                   }
{                                                                              }
{$IFDEF VER280}
  {$DEFINE DELPHI}
  {$DEFINE DELPHIXE7}
  {$DEFINE DELPHIXE7_UP}
{$ENDIF}



{                                                                              }
{ Delphi XE8                                                                   }
{                                                                              }
{$IFDEF VER290}
  {$DEFINE DELPHI}
  {$DEFINE DELPHIXE8}
  {$DEFINE DELPHIXE8_UP}
{$ENDIF}



{                                                                              }
{ Delphi 10 Seattle                                                            }
{                                                                              }
{$IFDEF VER300}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI10}
  {$DEFINE DELPHI10_UP}
{$ENDIF}



{                                                                              }
{ Delphi 10.1 Berlin                                                           }
{                                                                              }
{$IFDEF VER310}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI10_1}
  {$DEFINE DELPHI10_1_UP}
{$ENDIF}



{                                                                              }
{ Delphi 10.2 Tokyo                                                            }
{                                                                              }
{$IFDEF VER320}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI10_2}
  {$DEFINE DELPHI10_2_UP}
{$ENDIF}



{                                                                              }
{ Delphi 10.3 Rio                                                              }
{                                                                              }
{$IFDEF VER330}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI10_3}
  {$DEFINE DELPHI10_3_UP}
{$ENDIF}



{                                                                              }
{ Delphi 10.4 Sydney                                                           }
{                                                                              }
{$IFDEF VER340}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI10_4}
  {$DEFINE DELPHI10_4_UP}
{$ENDIF}



{                                                                              }
{ Future Delphi versions                                                       }
{                                                                              }
{$IFDEF VER350}
  {$DEFINE DELPHI_Future}
{$ENDIF}
{$IFDEF VER360}
  {$DEFINE DELPHI_Future}
{$ENDIF}
{$IFDEF VER370}
  {$DEFINE DELPHI_Future}
{$ENDIF}
{$IFDEF VER380}
  {$DEFINE DELPHI_Future}
{$ENDIF}
{$IFDEF VER390}
  {$DEFINE DELPHI_Future}
{$ENDIF}
{$IFDEF VER400}
  {$DEFINE DELPHI_Future}
{$ENDIF}

{$IFDEF DELPHI_Future}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI10_4_UP}
{$ENDIF}


{                                                                              }
{ DELPHI_xx_UP defines                                                         }
{                                                                              }
{$IFDEF DELPHI10_4_UP}
  {$DEFINE DELPHI10_3_UP}
{$ENDIF}
{$IFDEF DELPHI10_3_UP}
  {$DEFINE DELPHI10_2_UP}
{$ENDIF}
{$IFDEF DELPHI10_2_UP}
  {$DEFINE DELPHI10_1_UP}
{$ENDIF}
{$IFDEF DELPHI10_1_UP}
  {$DEFINE DELPHI10_UP}
{$ENDIF}
{$IFDEF DELPHI10_UP}
  {$DEFINE DELPHIXE8_UP}
{$ENDIF}
{$IFDEF DELPHIXE8_UP}
  {$DEFINE DELPHIXE7_UP}
{$ENDIF}
{$IFDEF DELPHIXE7_UP}
  {$DEFINE DELPHIXE6_UP}
{$ENDIF}
{$IFDEF DELPHIXE6_UP}
  {$DEFINE DELPHIXE5_UP}
{$ENDIF}
{$IFDEF DELPHIXE5_UP}
  {$DEFINE DELPHIXE4_UP}
{$ENDIF}
{$IFDEF DELPHIXE4_UP}
  {$DEFINE DELPHIXE3_UP}
{$ENDIF}
{$IFDEF DELPHIXE3_UP}
  {$DEFINE DELPHIXE2_UP}
{$ENDIF}
{$IFDEF DELPHIXE2_UP}
  {$DEFINE DELPHIXE_UP}
{$ENDIF}
{$IFDEF DELPHIXE_UP}
  {$DEFINE DELPHI2010_UP}
{$ENDIF}
{$IFDEF DELPHI2010_UP}
  {$DEFINE DELPHI2009_UP}
{$ENDIF}
{$IFDEF DELPHI2009_UP}
  {$DEFINE DELPHI2007_UP}
{$ENDIF}
{$IFDEF DELPHI2007_UP}
  {$DEFINE DELPHI2006_UP}
{$ENDIF}
{$IFDEF DELPHI2006_UP}
  {$DEFINE DELPHI2005_UP}
{$ENDIF}
{$IFDEF DELPHI2005_UP}
  {$DEFINE DELPHI8_UP}
{$ENDIF}
{$IFDEF DELPHI8_UP}
  {$DEFINE DELPHI7_UP}
{$ENDIF}
{$IFDEF DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
{$ENDIF}
{$IFDEF DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
{$ENDIF}
{$IFDEF DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
{$ENDIF}
{$IFDEF DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
{$ENDIF}
{$IFDEF DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
{$ENDIF}



{                                                                              }
{ Delphi: Operating System and CPU                                             }
{                                                                              }
{$IFDEF DELPHI}
  // Delphi 1 - Delphi 8
  {$IFDEF DELPHI1}
    {$DEFINE OS_WIN16}
    {$DEFINE CPU_X86}
  {$ENDIF}
  {$IFDEF DELPHI2}
    {$DEFINE OS_WIN32}
    {$DEFINE CPU_386}
  {$ENDIF}
  {$IFDEF DELPHI3}
    {$DEFINE OS_WIN32}
    {$DEFINE CPU_386}
  {$ENDIF}
  {$IFDEF DELPHI4}
    {$DEFINE OS_WIN32}
    {$DEFINE CPU_386}
  {$ENDIF}
  {$IFDEF DELPHI5}
    {$DEFINE OS_WIN32}
    {$DEFINE CPU_386}
  {$ENDIF}
  {$IFDEF DELPHI6}
    {$IFDEF LINUX}
      {$DEFINE OS_LINUX}
    {$ELSE}
      {$DEFINE OS_WIN32}
    {$ENDIF}
    {$DEFINE CPU_386}
  {$ENDIF}
  {$IFDEF DELPHI7}
    {$IFDEF LINUX}
      {$DEFINE OS_LINUX}
    {$ELSE}
      {$DEFINE OS_WIN32}
    {$ENDIF}
    {$DEFINE CPU_386}
  {$ENDIF}
  {$IFDEF DELPHI8}
    {$DEFINE OS_WIN32}
  {$ENDIF}
  {$IFNDEF DELPHIXE2_UP}
    // Delphi 2005+
    {$IFDEF DELPHI2005_UP}
      {$IFDEF WIN64}
        {$DEFINE OS_WIN64}
        {$DEFINE CPU_X86_64}
      {$ELSE}
        {$DEFINE OS_WIN32}
        {$DEFINE CPU_386}
      {$ENDIF}
    {$ENDIF}
  {$ELSE}
    // Delphi XE2+
    {$IFDEF CPUX64}
      {$DEFINE CPU_X86_64}
    {$ENDIF}
    {$IFDEF CPUX86}
      {$DEFINE CPU_386}
    {$ENDIF}
    {$IFDEF MACOS}
      {$DEFINE OS_MACOSX}
    {$ENDIF}
    {$IFDEF MACOS32}
      {$DEFINE OS_MACOSX32}
    {$ENDIF}
    {$IFDEF WIN64}
      {$DEFINE OS_WIN64}
    {$ENDIF}
    {$IFDEF POSIX}
      {$DEFINE OS_POSIX}
    {$ENDIF}
    {$IFDEF POSIX32}
      {$DEFINE OS_POSIX32}
    {$ENDIF}
    {$IFDEF POSIX64}
      {$DEFINE OS_POSIX64}
    {$ENDIF}
    // Delphi XE3+
    {$IFDEF DELPHIXE3_UP}
      {$IFDEF IOS}
        {$DEFINE OS_IOS}
      {$ENDIF}
      {$IFDEF IOS32}
        {$DEFINE OS_IOS32}
      {$ENDIF}
      {$IFDEF IOS64}
        {$DEFINE OS_IOS64}
      {$ENDIF}
      {$IFDEF CPUARM}
        {$DEFINE CPU_ARM}
      {$ENDIF}
    {$ENDIF}
    // Delphi XE5+
    {$IFDEF DELPHIXE5_UP}
      {$IFDEF ANDROID}
        {$DEFINE OS_ANDROID}
      {$ENDIF}
      {$IFDEF ANDROID32}
        {$DEFINE OS_ANDROID32}
      {$ENDIF}
    {$ENDIF}
    // Delphi 10.2+
    {$IFDEF DELPHI10_2_UP}
      {$IFDEF LINUX}
        {$DEFINE OS_LINUX}
      {$ENDIF}
      {$IFDEF LINUX32}
        {$DEFINE OS_LINUX32}
      {$ENDIF}
      {$IFDEF LINUX64}
        {$DEFINE OS_LINUX64}
      {$ENDIF}
      {$IFDEF MACOS64}
        {$DEFINE OS_MACOSX64}
      {$ENDIF}
    {$ENDIF}
    // Delphi 10.4+
    {$IFDEF DELPHI10_4_UP}
      {$IFDEF ANDROID64}
        {$DEFINE OS_ANDROID64}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}



{                                                                              }
{ Free Pascal: Operating System and CPU                                        }
{                                                                              }
{$IFDEF FREEPASCAL}
  {$IFDEF LINUX}   {$DEFINE OS_LINUX}   {$ENDIF}
  {$IFDEF FREEBSD} {$DEFINE OS_FREEBSD} {$ENDIF}
  {$IFDEF NETBSD}  {$DEFINE OS_NETBSD}  {$ENDIF}
  {$IFDEF SOLARIS} {$DEFINE OS_SOLARIS} {$ENDIF}
  {$IFDEF GO32V2}  {$DEFINE OS_DOS32}   {$ENDIF}
  {$IFDEF OS2}     {$DEFINE OS_OS2}     {$ENDIF}
  {$IFDEF WINDOWS} {$DEFINE OS_MSWIN}   {$ENDIF}
  {$IFDEF WIN32}   {$DEFINE OS_WIN32}   {$ENDIF}
  {$IFDEF WIN64}   {$DEFINE OS_WIN64}   {$ENDIF}
  {$IFDEF WINCE}   {$DEFINE OS_WINCE}   {$ENDIF}
  {$IFDEF DARWIN}  {$DEFINE OS_MACOSX}  {$ENDIF}

  {$IFDEF CPU386}       {$DEFINE CPU_386}    {$ENDIF}
  {$IFDEF CPUI386}      {$DEFINE CPU_386}    {$ENDIF}
  {$IFDEF CPUX86_64}    {$DEFINE CPU_X86_64} {$ENDIF}
  {$IFDEF CPUAMD64}     {$DEFINE CPU_X86_64} {$ENDIF}
  {$IFDEF ARM}          {$DEFINE CPU_ARM}    {$ENDIF}
  {$IFDEF CPU68}        {$DEFINE CPU_68K}    {$ENDIF}
  {$IFDEF CPUM68K}      {$DEFINE CPU_68K}    {$ENDIF}
  {$IFDEF CPUPOWERPC32} {$DEFINE CPU_PPC32}  {$ENDIF}
  {$IFDEF CPUPOWERPC64} {$DEFINE CPU_PPC64}  {$ENDIF}
{$ENDIF}



{                                                                              }
{ Operating System                                                             }
{                                                                              }

{ OS_WINxx }

{$IFDEF WIN16}
  {$DEFINE OS_WIN16}
{$ENDIF}
{$IFDEF WIN32}
  {$DEFINE OS_WIN32}
{$ENDIF}
{$IFDEF WIN64}
  {$DEFINE OS_WIN64}
{$ENDIF}

{ OS_MSWIN }

{$IFDEF OS_WIN16}
  {$DEFINE OS_MSWIN}
{$ENDIF}
{$IFDEF OS_WIN32}
  {$DEFINE OS_MSWIN}
{$ENDIF}
{$IFDEF OS_WIN64}
  {$DEFINE OS_MSWIN}
{$ENDIF}
{$IFDEF MSWINDOWS}
  {$DEFINE OS_MSWIN}
{$ENDIF}

{ OS_MSWIN aliases }

{$IFDEF OS_MSWIN}
  {$DEFINE MSWIN}
  {$DEFINE OS_WIN}
  {$DEFINE WIN}
{$ENDIF}

{ OS Posix }

{$IFDEF MACOSX}
  {$DEFINE OS_MACOSX}
{$ENDIF}
{$IFDEF LINUX}
  {$DEFINE OS_LINUX}
{$ENDIF}
{$IFDEF FREEBSD}
  {$DEFINE OS_FREEBSD}
{$ENDIF}
{$IFDEF NETBSD}
  {$DEFINE OS_NETBSD}
{$ENDIF}
{$IFDEF OPENBSD}
  {$DEFINE OS_OPENBSD}
{$ENDIF}
{$IFDEF POSIX}
  {$DEFINE OS_POSIX}
{$ENDIF}
{$IFDEF POSIX32}
  {$DEFINE OS_POSIX32}
{$ENDIF}
{$IFDEF POSIX64}
  {$DEFINE OS_POSIX64}
{$ENDIF}

{ OS_BSD, OS_POSIX }

{$IFDEF OS_MACOSX}
  {$DEFINE OS_POSIX}
{$ENDIF}
{$IFDEF OS_LINUX}
  {$DEFINE OS_POSIX}
{$ENDIF}
{$IFDEF OS_FREEBSD}
  {$DEFINE OS_BSD}
  {$DEFINE OS_POSIX}
{$ENDIF}
{$IFDEF OS_NETBSD}
  {$DEFINE OS_BSD}
  {$DEFINE OS_POSIX}
{$ENDIF}
{$IFDEF OS_OPENBSD}
  {$DEFINE OS_BSD}
  {$DEFINE OS_POSIX}
{$ENDIF}

{$IFDEF OS_BSD}
  {$DEFINE BSD}
{$ENDIF}

{$IFDEF OS_POSIX}
  {$DEFINE POSIX}
{$ENDIF}

{ OS_MOBILE }

{$IFDEF OS_ANDROID}
  {$DEFINE OS_MOBILE}
{$ENDIF}
{$IFDEF OS_IOS}
  {$DEFINE OS_MOBILE}
{$ENDIF}

{ OS bits }

{$IFDEF OS_DOS32}
  {$DEFINE OS_32}
{$ENDIF}
{$IFDEF OS_WIN32}
  {$DEFINE OS_32}
{$ENDIF}
{$IFDEF OS_WIN64}
  {$DEFINE OS_64}
{$ENDIF}
{$IFDEF OS_MACOSX32}
  {$DEFINE OS_32}
{$ENDIF}
{$IFDEF OS_MACOSX64}
  {$DEFINE OS_64}
{$ENDIF}
{$IFDEF OS_LINUX32}
  {$DEFINE OS_32}
{$ENDIF}
{$IFDEF OS_LINUX64}
  {$DEFINE OS_64}
{$ENDIF}
{$IFDEF OS_ANDROID32}
  {$DEFINE OS_32}
{$ENDIF}
{$IFDEF OS_ANDROID64}
  {$DEFINE OS_64}
{$ENDIF}
{$IFDEF OS_IOS32}
  {$DEFINE OS_32}
{$ENDIF}
{$IFDEF OS_IOS64}
  {$DEFINE OS_64}
{$ENDIF}

{ Other }

{$IFDEF OS2}
  {$DEFINE OS_OS2}
{$ENDIF}
{$IFDEF DOS32}
  {$DEFINE DOS}
  {$DEFINE OS_DOS32}
{$ENDIF}
{$IFDEF DOS}
  {$DEFINE OS_DOS}
{$ENDIF}



{                                                                              }
{ .NET                                                                         }
{                                                                              }
{$IFDEF CLR}
  {$DEFINE DOT_NET}
{$ENDIF}
{$IFDEF DOT_NET}
  {$DEFINE ManagedCode}
  {$DEFINE ExtendedIsDouble}
{$ENDIF}



{                                                                              }
{ Windows platform                                                             }
{                                                                              }
{$IFDEF DOT_NET}
  {$DEFINE WindowsPlatform}
{$ENDIF}
{$IFDEF OS_WIN32}
  {$DEFINE WindowsPlatform}
  {$DEFINE WIN32}
{$ENDIF}
{$IFDEF OS_WIN64}
  {$DEFINE WindowsPlatform}
  {$DEFINE WIN64}
{$ENDIF}



{                                                                              }
{ CPU type                                                                     }
{                                                                              }
{$IFNDEF ManagedCode}
  {$DEFINE NativeCode}
{$ENDIF}

{ CPU types }

{$IFDEF CPU386}
  {$DEFINE INTEL386}
  {$DEFINE CPU_INTEL386}
{$ENDIF}
{$IFDEF CPUX64}
  {$DEFINE CPU_X86_64}
{$ENDIF}
{$IFDEF CPU86_64}
  {$DEFINE CPU_X86_64}
{$ENDIF}
{$IFDEF CPU68K}
  {$DEFINE CPU_68K}
{$ENDIF}
{$IFDEF CPUPPC}
  {$DEFINE CPU_PPC32}
{$ENDIF}
{$IFDEF CPUPPC64}
  {$DEFINE CPU_PPC64}
{$ENDIF}
{$IFDEF CPUARM}
  {$DEFINE CPU_ARM}
{$ENDIF}
{$IFDEF CPUARM32}
  {$DEFINE CPU_ARM32}
{$ENDIF}
{$IFDEF CPUARM64}
  {$DEFINE CPU_ARM64}
{$ENDIF}

{ CPU bits }

{$IFDEF CPU32BITS}
  {$DEFINE CPU_32}
{$ENDIF}
{$IFDEF CPU64BITS}
  {$DEFINE CPU_64}
{$ENDIF}
{$IFDEF CPU_INTEL386}
  {$DEFINE CPU_32}
{$ENDIF}
{$IFDEF CPU_X86_64}
  {$DEFINE CPU_64}
{$ENDIF}
{$IFDEF CPU_PPC64}
  {$DEFINE CPU_64}
{$ENDIF}
{$IFDEF CPU_PPC32}
  {$DEFINE CPU_32}
{$ENDIF}
{$IFDEF CPU_ARM32}
  {$DEFINE CPU_32}
{$ENDIF}
{$IFDEF CPU_ARM64}
  {$DEFINE CPU_64}
{$ENDIF}

{$IFNDEF CPU_32}
{$IFNDEF CPU_64}
  {$IFDEF OS_64}
    {$DEFINE CPU_64}
  {$ENDIF}
  {$IFDEF OS_32}
    {$DEFINE CPU_32}
  {$ENDIF}
{$ENDIF}
{$ENDIF}



{                                                                              }
{ Delphi: Supported language features                                          }
{                                                                              }
{$IFDEF DELPHI}
  {$DEFINE SupportShortString}
  {$DEFINE SetSizeAlignIsByte}
{$ENDIF}

{$IFDEF DELPHI2_UP}
  {$DEFINE SupportAnsiChar}
  {$DEFINE SupportAnsiString}
{$ENDIF}

{$IFDEF DELPHI3_UP}
  {$DEFINE SupportCardinal}
  {$DEFINE SupportCurrency}
  {$DEFINE SupportVariant}
  {$DEFINE ExtendedIs80Bits}
  {$DEFINE LongWordIs32Bits}
  {$DEFINE LongIntIs32Bits}
{$ENDIF}

{$IFDEF DELPHI4_UP}
  {$DEFINE SupportInt64}
  {$DEFINE SupportWideChar}
  {$DEFINE SupportWideString}
  {$DEFINE SupportDynArray}
  {$DEFINE SupportAssert}
{$ENDIF}

{$IFDEF DELPHI5_UP}
  {$DEFINE SupportOverload}
{$ENDIF}

{$IFDEF DELPHI6_UP}
  {$DEFINE SupportInterface}
  {$DEFINE SupportUCS4String}
  {$DEFINE SupportUCS4Char}
  {$DEFINE SupportEnumValue}
{$ENDIF}

{$IFDEF DELPHI2005_UP}
  {$DEFINE SupportInline}
  {$DEFINE SupportOperatorOverloading}
{$ENDIF}

{$IFDEF DELPHI2005}
  {$DEFINE SupportInlineIsBuggy}
{$ENDIF}

{$IFDEF DELPHI2006}
  {$DEFINE SupportInlineIsBuggy}
{$ENDIF}

{$IFDEF DELPHI2007_UP}
  {$DEFINE SupportForIn}
  {$DEFINE TBytesDeclared}
{$ENDIF}

{$IFDEF DELPHI2009_UP}
  {$DEFINE SupportAnonymousMethods}
  {$DEFINE SupportGenerics}
  {$DEFINE SupportUnicodeString}
  {$DEFINE SupportRawByteString}
  {$DEFINE SupportUTF8String}
  {$IFDEF ManagedCode}
    {$UNDEF SupportRawByteString}
    {$UNDEF SupportUTF8String}
    {$UNDEF SupportUTF8Char}
    {$UNDEF SupportUCS4String}
    {$UNDEF SupportUCS4Char}
  {$ENDIF}
  {$DEFINE SupportNativeInt}
  {$DEFINE SupportNativeUInt}
  {$IFDEF CPU_64}
    {$DEFINE NativeIntIs64Bits}
    {$DEFINE NativeUIntIs64Bits}
  {$ELSE}
    {$IFDEF CPU_32}
      {$DEFINE NativeIntIs32Bits}
      {$DEFINE NativeUIntIs32Bits}
    {$ENDIF}
  {$ENDIF}
  {$DEFINE SupportUInt64}
  {$DEFINE SupportStringRefCount}
{$ENDIF}

{$IFDEF DELPHI2009_UP}
  {$DEFINE CharIsWide}
  {$DEFINE StringIsUnicode}
{$ELSE}
  {$DEFINE CharIsAnsi}
  {$DEFINE StringIsAnsi}
{$ENDIF}

{$IFDEF DELPHIXE2_UP}
  {$IFNDEF WIN32}
    {$UNDEF ExtendedIs80Bits}
    {$DEFINE ExtendedIsDouble}
  {$ENDIF}
{$ENDIF}

{$IFDEF DELPHIXE4_UP}
  {$IFDEF LINUX}
    {$UNDEF ExtendedIs80Bits}
    {$UNDEF ExtendedIsDouble}
    {$DEFINE ExtendedIs16Bytes}
  {$ENDIF}
  {$IFDEF IOS}
    {$UNDEF SupportAnsiChar}
    {$UNDEF SupportAnsiString}
    {$UNDEF SupportWideString}
    {$UNDEF SupportShortString}
  {$ENDIF}
  {$IFDEF ANDROID}
    {$UNDEF SupportAnsiChar}
    {$UNDEF SupportAnsiString}
    {$UNDEF SupportWideString}
    {$UNDEF SupportShortString}
  {$ENDIF}
{$ENDIF}

{$IFDEF DELPHIXE4_UP}
  {$IFDEF AUTOREFCOUNT}
    {$DEFINE ObjectARC}
  {$ENDIF}
{$ENDIF}

{$IFDEF DELPHIXE8_UP}
  {$DEFINE SupportFixedInt}
  {$DEFINE SupportFixedUInt}
  {$IFDEF LINUX}
    {$UNDEF SupportAnsiChar}
    {$UNDEF SupportAnsiString}
    {$UNDEF SupportWideString}
    {$UNDEF SupportShortString}
  {$ENDIF}
  {$IFDEF OS_IOS64}
    {$UNDEF LongWordIs32Bits}
    {$UNDEF LongIntIs32Bits}
    {$DEFINE LongWordIs64Bits}
    {$DEFINE LongIntIs64Bits}
  {$ENDIF}
{$ENDIF}

{$IFDEF DELPHI10_UP}
  {$DEFINE SupportUTF8Char}
  {$IFDEF MACOS32}
    {$UNDEF ExtendedIs80Bits}
    {$UNDEF ExtendedIsDouble}
    {$DEFINE ExtendedIs16Bytes}
  {$ENDIF}
{$ENDIF}

{$IFDEF DELPHI10_3_UP}
  {$IFDEF LINUX}
    {$DEFINE SupportAnsiChar}
    {$DEFINE SupportAnsiString}
  {$ENDIF}
{$ENDIF}

{$IFDEF DELPHI10_4_UP}
  {$DEFINE SupportAnsiChar}
  {$DEFINE SupportAnsiString}
{$ENDIF}



{                                                                              }
{ Delphi: Switches                                                             }
{                                                                              }
{$IFDEF DELPHIXE4_UP}
  {$IFDEF IOS}
    {$ZEROBASEDSTRINGS OFF}
  {$ENDIF}
  {$IFDEF ANDROID}
    {$ZEROBASEDSTRINGS OFF}
  {$ENDIF}
{$ENDIF}



{                                                                              }
{ Free Pascal                                                                  }
{                                                                              }
{$IFDEF FPC}

  {$DEFINE FREEPASCAL}

  {$IFDEF VER1}
    {$DEFINE FREEPASCAL1}
  {$ELSE}
    {$DEFINE FREEPASCAL2_UP}

    {$IFDEF VER2}
      {$DEFINE FREEPASCAL2}
    {$ENDIF}

    {$IFDEF VER2_0}
      {$DEFINE FREEPASCAL2_0}
    {$ENDIF}
    {$IFDEF VER2_2}
      {$DEFINE FREEPASCAL2_2}
    {$ENDIF}
    {$IFDEF VER2_4}
      {$DEFINE FREEPASCAL2_4}
    {$ENDIF}
    {$IFDEF VER2_6}
      {$DEFINE FREEPASCAL2_6}
      {$DEFINE FREEPASCAL2_6_UP}
    {$ENDIF}
    {$IFDEF VER2_7}
      {$DEFINE FREEPASCAL2_7}
      {$DEFINE FREEPASCAL2_6_UP}
      {$DEFINE FREEPASCAL2_7_UP}
    {$ENDIF}

    {$IFDEF VER3_0}
      {$DEFINE FREEPASCAL3}
      {$DEFINE FREEPASCAL3_UP}
      {$DEFINE FREEPASCAL3_0}
      {$DEFINE FREEPASCAL2_6_UP}
      {$DEFINE FREEPASCAL2_7_UP}
      {$DEFINE FREEPASCAL3_0_UP}
    {$ENDIF}
    {$IFDEF VER3_1}
      {$DEFINE FREEPASCAL3}
      {$DEFINE FREEPASCAL3_UP}
      {$DEFINE FREEPASCAL3_1}
      {$DEFINE FREEPASCAL2_6_UP}
      {$DEFINE FREEPASCAL2_7_UP}
      {$DEFINE FREEPASCAL3_0_UP}
      {$DEFINE FREEPASCAL3_1_UP}
    {$ENDIF}
    {$IFDEF VER3_2}
      {$DEFINE FREEPASCAL3}
      {$DEFINE FREEPASCAL3_UP}
      {$DEFINE FREEPASCAL3_2}
      {$DEFINE FREEPASCAL2_6_UP}
      {$DEFINE FREEPASCAL2_7_UP}
      {$DEFINE FREEPASCAL3_0_UP}
      {$DEFINE FREEPASCAL3_1_UP}
      {$DEFINE FREEPASCAL3_2_UP}
    {$ENDIF}
  {$ENDIF}

{$ENDIF}



{                                                                              }
{ FreePascal: Mode                                                             }
{                                                                              }
{$IFDEF FREEPASCAL2}
  {$MODE DELPHI}
{$ENDIF}

{$IFDEF FREEPASCAL3_UP}
  {$MODE DELPHIUNICODE}
{$ENDIF}



{                                                                              }
{ FreePascal: Supported language features                                      }
{                                                                              }
{$IFDEF FREEPACAL2}
  {$DEFINE CharIsAnsi}
  {$DEFINE StringIsAnsi}
{$ENDIF}

{$IFDEF FREEPASCAL2_UP}
  {$DEFINE SupportNativeInt}
  {$DEFINE SupportNativeUInt}
  {$DEFINE SupportInt64}
  {$DEFINE SupportUInt64}
  {$DEFINE SupportAnsiChar}
  {$DEFINE SupportAnsiString}
  {$DEFINE SupportCardinal}
  {$DEFINE SupportCurrency}
  {$DEFINE SupportWideChar}
  {$DEFINE SupportWideString}
  {$DEFINE SupportDynArray}
  {$DEFINE SupportAssert}
  {$DEFINE SupportInterface}
  {$DEFINE SupportOverload}
  {$DEFINE SupportUTF8String}
  {$DEFINE SupportUCS4String}
  {$IFDEF FREEPASCAL2_6_UP}
    {$DEFINE SetSizeAlignIsByte}
  {$ELSE}
    {$IFDEF FREEPASCAL2_UP}
      {$DEFINE SetSizeAlignIsLongWord}
    {$ELSE}
      {$DEFINE SetSizeAlignIsByte}
    {$ENDIF}
  {$ENDIF}
  {$DEFINE SupportEnumValue}
  {$DEFINE LongWordIs32Bits}
  {$DEFINE LongIntIs32Bits}
  {$IFDEF CPU_64}
    {$DEFINE NativeIntIs64Bits}
    {$DEFINE NativeUIntIs64Bits}
  {$ELSE}
    {$IFDEF CPU_32}
      {$DEFINE NativeIntIs32Bits}
      {$DEFINE NativeUIntIs32Bits}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF FREEPASCAL3_UP}
  {$DEFINE CharIsWide}
  {$DEFINE StringIsUnicode}
  {$DEFINE SupportUnicodeString}
  {$DEFINE SupportRawByteString}
  {$DEFINE SupportStringRefCount}

  {$IFDEF CPUX86_64}
  {$IFNDEF LINUX}
    {$DEFINE ExtendedIsDouble}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}



{                                                                              }
{ Function inlining                                                            }
{                                                                              }
{$IFDEF SupportInline}
  {$IFNDEF SupportInlineIsBuggy}
    {$IFNDEF PurePascal}
      {$DEFINE UseInline}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}



{                                                                              }
{ Assembler style                                                              }
{                                                                              }
{$IFNDEF PurePascal}
  {$IFNDEF ManagedCode}
    {$IFDEF CPU_X86_64}
      {$DEFINE ASMX86_64}
      {$IFDEF DELPHI}
        {$DEFINE ASMX86_64_DELPHI}
      {$ENDIF}
    {$ENDIF}
    {$IFDEF CPU_INTEL386}
      {$DEFINE ASM386}
      {$IFDEF DELPHI}{$IFDEF OS_WIN32}
        {$DEFINE ASM386_DELPHI}
        {$IFNDEF UseInline} {$DEFINE ASM386_DELPHI_INLINE_OFF} {$ENDIF}
      {$ENDIF}{$ENDIF}
      {$IFDEF FREEPASCAL2_UP}
        {$DEFINE ASM386_FREEPASCAL}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}



{                                                                              }
{ Framework                                                                    }
{                                                                              }
{$IFDEF DELPHIXE2_UP}
  {$IFDEF CONSOLE}
    {$DEFINE IsConsoleApp}
  {$ENDIF}
{$ENDIF}



{                                                                              }
{ Standard compiler directives                                                 }
{                                                                              }
{$EXTENDEDSYNTAX ON}
{$IOCHECKS ON}
{$LONGSTRINGS ON}
{$BOOLEVAL OFF}
{$WRITEABLECONST OFF}
{$MINENUMSIZE 1}
{$IFDEF DEBUG}
  {$ASSERTIONS ON}
  {$DEBUGINFO ON}
  {$OVERFLOWCHECKS ON}
  {$RANGECHECKS ON}
  {$WARNINGS ON}
  {$HINTS ON}
{$ELSE}
  {$ASSERTIONS OFF}
  {$DEBUGINFO OFF}
  {$OVERFLOWCHECKS OFF}
  {$RANGECHECKS OFF}
  {$WARNINGS OFF}
  {$HINTS OFF}
{$ENDIF}
{$IFDEF CLR}
  {$UNSAFECODE OFF}
{$ENDIF}
{$IFDEF DELPHI}
  {$OPTIMIZATION ON}
{$ENDIF}
{$IFDEF DELPHI2005_UP}
  {$INLINE ON}
{$ENDIF}
{$IFDEF DELPHI2009_UP}
  {$HIGHCHARUNICODE OFF}
{$ENDIF}



{                                                                              }
{ Compiler warnings                                                            }
{                                                                              }
{$IFDEF DELPHI7}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFDEF DELPHI2007}
{$IFNDEF DOT_NET}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}
{$ENDIF}

{$IFDEF DELPHIXE2_UP}
  {$WARN DUPLICATE_CTOR_DTOR OFF}
{$ENDIF}

{$IFDEF DOT_NET}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}

{$IFNDEF DEBUG}
  {$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM OFF}
    {$WARN UNIT_PLATFORM OFF}
    {$WARN UNIT_DEPRECATED OFF}
  {$ENDIF}

{$ENDIF}

{$IFDEF FREEPASCAL}
  {$WARN 4055 OFF} // Conversion between ordinals and pointers is not portable
{$ENDIF}

