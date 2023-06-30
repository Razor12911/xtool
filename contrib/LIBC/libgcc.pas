unit libgcc;
interface
{$IFDEF WIN64}
procedure __alloca;external;
{$L x64/_chkstk.o}
procedure ___chkstk_ms;external;
{$L x64/_chkstk_ms.o}
procedure __multi3;external;
{$L x64/_muldi3.o}
procedure __negti2;external;
{$L x64/_negdi2.o}
procedure __lshrti3;external;
{$L x64/_lshrdi3.o}
procedure __ashlti3;external;
{$L x64/_ashldi3.o}
procedure __ashrti3;external;
{$L x64/_ashrdi3.o}
procedure __cmpti2;external;
{$L x64/_cmpdi2.o}
procedure __ucmpti2;external;
{$L x64/_ucmpdi2.o}
procedure __clear_cache;external;
{$L x64/_clear_cache.o}
procedure getpagesize;external;
{$L x64/_trampoline.o}
procedure __do_global_dtors;external;
{$L x64/__main.o}
procedure __absvdi2;external;
{$L x64/_absvsi2.o}
procedure __absvti2;external;
{$L x64/_absvdi2.o}
procedure __addvdi3;external;
{$L x64/_addvsi3.o}
procedure __addvti3;external;
{$L x64/_addvdi3.o}
procedure __subvdi3;external;
{$L x64/_subvsi3.o}
procedure __subvti3;external;
{$L x64/_subvdi3.o}
procedure __mulvdi3;external;
{$L x64/_mulvsi3.o}
procedure __mulvti3;external;
{$L x64/_mulvdi3.o}
procedure __negvdi2;external;
{$L x64/_negvsi2.o}
procedure __negvti2;external;
{$L x64/_negvdi2.o}
procedure __ffsdi2;external;
{$L x64/_ffssi2.o}
procedure __ffsti2;external;
{$L x64/_ffsdi2.o}
procedure __clzdi2;external;
{$L x64/_clzsi2.o}
procedure __clzti2;external;
{$L x64/_clzdi2.o}
procedure __ctzdi2;external;
{$L x64/_ctzsi2.o}
procedure __ctzti2;external;
{$L x64/_ctzdi2.o}
procedure __popcountdi2;external;
{$L x64/_popcountsi2.o}
procedure __popcountti2;external;
{$L x64/_popcountdi2.o}
procedure __paritydi2;external;
{$L x64/_paritysi2.o}
procedure __parityti2;external;
{$L x64/_paritydi2.o}
procedure __powisf2;external;
{$L x64/_powisf2.o}
procedure __powidf2;external;
{$L x64/_powidf2.o}
procedure __powixf2;external;
{$L x64/_powixf2.o}
procedure __powitf2;external;
{$L x64/_powitf2.o}
procedure __mulsc3;external;
{$L x64/_mulsc3.o}
procedure __muldc3;external;
{$L x64/_muldc3.o}
procedure __mulxc3;external;
{$L x64/_mulxc3.o}
procedure __multc3;external;
{$L x64/_multc3.o}
procedure __divsc3;external;
{$L x64/_divsc3.o}
procedure __divdc3;external;
{$L x64/_divdc3.o}
procedure __divxc3;external;
{$L x64/_divxc3.o}
procedure __divtc3;external;
{$L x64/_divtc3.o}
procedure __bswapsi2;external;
{$L x64/_bswapsi2.o}
procedure __bswapdi2;external;
{$L x64/_bswapdi2.o}
procedure __clrsbdi2;external;
{$L x64/_clrsbsi2.o}
procedure __clrsbti2;external;
{$L x64/_clrsbdi2.o}
procedure __fixunssfdi;external;
{$L x64/_fixunssfsi.o}
procedure __fixunsdfdi;external;
{$L x64/_fixunsdfsi.o}
procedure __fixunsxfdi;external;
{$L x64/_fixunsxfsi.o}
procedure __fixsfti;external;
{$L x64/_fixsfdi.o}
procedure __fixdfti;external;
{$L x64/_fixdfdi.o}
procedure __fixxfti;external;
{$L x64/_fixxfdi.o}
procedure __fixunssfti;external;
{$L x64/_fixunssfdi.o}
procedure __fixunsdfti;external;
{$L x64/_fixunsdfdi.o}
procedure __fixunsxfti;external;
{$L x64/_fixunsxfdi.o}
procedure __floattisf;external;
{$L x64/_floatdisf.o}
procedure __floattidf;external;
{$L x64/_floatdidf.o}
procedure __floattixf;external;
{$L x64/_floatdixf.o}
procedure __floatuntisf;external;
{$L x64/_floatundisf.o}
procedure __floatuntidf;external;
{$L x64/_floatundidf.o}
procedure __floatuntixf;external;
{$L x64/_floatundixf.o}
procedure __gcc_bcmp;external;
{$L x64/__gcc_bcmp.o}
procedure __divti3;external;
{$L x64/_divdi3.o}
procedure __modti3;external;
{$L x64/_moddi3.o}
procedure __divmodti4;external;
{$L x64/_divmoddi4.o}
procedure __udivti3;external;
{$L x64/_udivdi3.o}
procedure __umodti3;external;
{$L x64/_umoddi3.o}
procedure __udivmodti4;external;
{$L x64/_udivmoddi4.o}
procedure __udiv_w_sdiv;external;
{$L x64/_udiv_w_sdiv.o}
procedure __dfp_set_round;external;
{$L x64/bid_decimal_globals.o}
procedure __bid32_to_binary32;external;
{$L x64/bid_binarydecimal.o}
procedure isinfd32;external;
{$L x64/_isinfd32.o}
procedure isinfd64;external;
{$L x64/_isinfd64.o}
procedure isinfd128;external;
{$L x64/_isinfd128.o}
procedure __bid64_isSigned;external;
{$L x64/bid64_noncomp.o}
procedure __bid128_isSigned;external;
{$L x64/bid128_noncomp.o}
procedure __bid_round64_2_18;external;
{$L x64/bid_round.o}
procedure __bid64_from_int32;external;
{$L x64/bid_from_int.o}
procedure __bid64_add;external;
{$L x64/bid64_add.o}
procedure __bid64dq_add;external;
{$L x64/bid128_add.o}
procedure __bid64_mul;external;
{$L x64/bid64_mul.o}
procedure __bid64qq_mul;external;
{$L x64/bid128_mul.o}
procedure __bid64_quiet_equal;external;
{$L x64/bid64_compare.o}
procedure __bid128_quiet_equal;external;
{$L x64/bid128_compare.o}
procedure __bid32_to_bid64;external;
{$L x64/bid32_to_bid64.o}
procedure __bid32_to_bid128;external;
{$L x64/bid32_to_bid128.o}
procedure __bid64_to_bid128;external;
{$L x64/bid64_to_bid128.o}
procedure __bid64_to_int32_rnint;external;
{$L x64/bid64_to_int32.o}
procedure __bid64_to_int64_rnint;external;
{$L x64/bid64_to_int64.o}
procedure __bid64_to_uint32_rnint;external;
{$L x64/bid64_to_uint32.o}
procedure __bid64_to_uint64_rnint;external;
{$L x64/bid64_to_uint64.o}
procedure __bid128_to_int32_rnint;external;
{$L x64/bid128_to_int32.o}
procedure __bid128_to_int64_rnint;external;
{$L x64/bid128_to_int64.o}
procedure __bid128_to_uint32_rnint;external;
{$L x64/bid128_to_uint32.o}
procedure __bid128_to_uint64_rnint;external;
{$L x64/bid128_to_uint64.o}
procedure __bid_addsd3;external;
{$L x64/_addsub_sd.o}
procedure __bid_divsd3;external;
{$L x64/_div_sd.o}
procedure __bid_mulsd3;external;
{$L x64/_mul_sd.o}
procedure __bid_eqsd2;external;
{$L x64/_eq_sd.o}
procedure __bid_nesd2;external;
{$L x64/_ne_sd.o}
procedure __bid_ltsd2;external;
{$L x64/_lt_sd.o}
procedure __bid_gtsd2;external;
{$L x64/_gt_sd.o}
procedure __bid_lesd2;external;
{$L x64/_le_sd.o}
procedure __bid_gesd2;external;
{$L x64/_ge_sd.o}
procedure __bid_fixsdsi;external;
{$L x64/_sd_to_si.o}
procedure __bid_fixsddi;external;
{$L x64/_sd_to_di.o}
procedure __bid_fixunssdsi;external;
{$L x64/_sd_to_usi.o}
procedure __bid_fixunssddi;external;
{$L x64/_sd_to_udi.o}
procedure __bid_floatsisd;external;
{$L x64/_si_to_sd.o}
procedure __bid_floatdisd;external;
{$L x64/_di_to_sd.o}
procedure __bid_floatunssisd;external;
{$L x64/_usi_to_sd.o}
procedure __bid_floatunsdisd;external;
{$L x64/_udi_to_sd.o}
procedure __bid_truncsdsf;external;
{$L x64/_sd_to_sf.o}
procedure __bid_extendsddf;external;
{$L x64/_sd_to_df.o}
procedure __bid_extendsdxf;external;
{$L x64/_sd_to_xf.o}
procedure __bid_extendsdtf;external;
{$L x64/_sd_to_tf.o}
procedure __bid_extendsfsd;external;
{$L x64/_sf_to_sd.o}
procedure __bid_truncdfsd;external;
{$L x64/_df_to_sd.o}
procedure __bid_truncxfsd;external;
{$L x64/_xf_to_sd.o}
procedure __bid_trunctfsd;external;
{$L x64/_tf_to_sd.o}
procedure __bid_extendsddd2;external;
{$L x64/_sd_to_dd.o}
procedure __bid_extendsdtd2;external;
{$L x64/_sd_to_td.o}
procedure __bid_unordsd2;external;
{$L x64/_unord_sd.o}
procedure __bid_adddd3;external;
{$L x64/_addsub_dd.o}
procedure __bid_divdd3;external;
{$L x64/_div_dd.o}
procedure __bid_muldd3;external;
{$L x64/_mul_dd.o}
procedure __bid_eqdd2;external;
{$L x64/_eq_dd.o}
procedure __bid_nedd2;external;
{$L x64/_ne_dd.o}
procedure __bid_ltdd2;external;
{$L x64/_lt_dd.o}
procedure __bid_gtdd2;external;
{$L x64/_gt_dd.o}
procedure __bid_ledd2;external;
{$L x64/_le_dd.o}
procedure __bid_gedd2;external;
{$L x64/_ge_dd.o}
procedure __bid_fixddsi;external;
{$L x64/_dd_to_si.o}
procedure __bid_fixdddi;external;
{$L x64/_dd_to_di.o}
procedure __bid_fixunsddsi;external;
{$L x64/_dd_to_usi.o}
procedure __bid_fixunsdddi;external;
{$L x64/_dd_to_udi.o}
procedure __bid_floatsidd;external;
{$L x64/_si_to_dd.o}
procedure __bid_floatdidd;external;
{$L x64/_di_to_dd.o}
procedure __bid_floatunssidd;external;
{$L x64/_usi_to_dd.o}
procedure __bid_floatunsdidd;external;
{$L x64/_udi_to_dd.o}
procedure __bid_truncddsf;external;
{$L x64/_dd_to_sf.o}
procedure __bid_truncdddf;external;
{$L x64/_dd_to_df.o}
procedure __bid_extendddxf;external;
{$L x64/_dd_to_xf.o}
procedure __bid_extendddtf;external;
{$L x64/_dd_to_tf.o}
procedure __bid_extendsfdd;external;
{$L x64/_sf_to_dd.o}
procedure __bid_extenddfdd;external;
{$L x64/_df_to_dd.o}
procedure __bid_truncxfdd;external;
{$L x64/_xf_to_dd.o}
procedure __bid_trunctfdd;external;
{$L x64/_tf_to_dd.o}
procedure __bid_truncddsd2;external;
{$L x64/_dd_to_sd.o}
procedure __bid_extendddtd2;external;
{$L x64/_dd_to_td.o}
procedure __bid_unorddd2;external;
{$L x64/_unord_dd.o}
procedure __bid_addtd3;external;
{$L x64/_addsub_td.o}
procedure __bid_divtd3;external;
{$L x64/_div_td.o}
procedure __bid_multd3;external;
{$L x64/_mul_td.o}
procedure __bid_eqtd2;external;
{$L x64/_eq_td.o}
procedure __bid_netd2;external;
{$L x64/_ne_td.o}
procedure __bid_lttd2;external;
{$L x64/_lt_td.o}
procedure __bid_gttd2;external;
{$L x64/_gt_td.o}
procedure __bid_letd2;external;
{$L x64/_le_td.o}
procedure __bid_getd2;external;
{$L x64/_ge_td.o}
procedure __bid_fixtdsi;external;
{$L x64/_td_to_si.o}
procedure __bid_fixtddi;external;
{$L x64/_td_to_di.o}
procedure __bid_fixunstdsi;external;
{$L x64/_td_to_usi.o}
procedure __bid_fixunstddi;external;
{$L x64/_td_to_udi.o}
procedure __bid_floatsitd;external;
{$L x64/_si_to_td.o}
procedure __bid_floatditd;external;
{$L x64/_di_to_td.o}
procedure __bid_floatunssitd;external;
{$L x64/_usi_to_td.o}
procedure __bid_floatunsditd;external;
{$L x64/_udi_to_td.o}
procedure __bid_trunctdsf;external;
{$L x64/_td_to_sf.o}
procedure __bid_trunctddf;external;
{$L x64/_td_to_df.o}
procedure __bid_trunctdxf;external;
{$L x64/_td_to_xf.o}
procedure __bid_trunctdtf;external;
{$L x64/_td_to_tf.o}
procedure __bid_extendsftd;external;
{$L x64/_sf_to_td.o}
procedure __bid_extenddftd;external;
{$L x64/_df_to_td.o}
procedure __bid_extendxftd;external;
{$L x64/_xf_to_td.o}
procedure __bid_extendtftd;external;
{$L x64/_tf_to_td.o}
procedure __bid_trunctdsd2;external;
{$L x64/_td_to_sd.o}
procedure __bid_trunctddd2;external;
{$L x64/_td_to_dd.o}
procedure __bid_unordtd2;external;
{$L x64/_unord_td.o}
procedure __sfp_handle_exceptions;external;
{$L x64/sfp-exceptions.o}
procedure __addtf3;external;
{$L x64/addtf3.o}
procedure __divtf3;external;
{$L x64/divtf3.o}
procedure __eqtf2;external;
{$L x64/eqtf2.o}
procedure __netf2;external;
{$L x64/eqtf2.o}
procedure __getf2;external;
{$L x64/getf2.o}
procedure __gttf2;external;
{$L x64/getf2.o}
procedure __letf2;external;
{$L x64/letf2.o}
procedure __lttf2;external;
{$L x64/letf2.o}
procedure __multf3;external;
{$L x64/multf3.o}
procedure __negtf2;external;
{$L x64/negtf2.o}
procedure __subtf3;external;
{$L x64/subtf3.o}
procedure __unordtf2;external;
{$L x64/unordtf2.o}
procedure __fixtfsi;external;
{$L x64/fixtfsi.o}
procedure __fixunstfsi;external;
{$L x64/fixunstfsi.o}
procedure __floatsitf;external;
{$L x64/floatsitf.o}
procedure __floatunsitf;external;
{$L x64/floatunsitf.o}
procedure __fixtfdi;external;
{$L x64/fixtfdi.o}
procedure __fixunstfdi;external;
{$L x64/fixunstfdi.o}
procedure __floatditf;external;
{$L x64/floatditf.o}
procedure __floatunditf;external;
{$L x64/floatunditf.o}
procedure __fixtfti;external;
{$L x64/fixtfti.o}
procedure __fixunstfti;external;
{$L x64/fixunstfti.o}
procedure __floattitf;external;
{$L x64/floattitf.o}
procedure __floatuntitf;external;
{$L x64/floatuntitf.o}
procedure __extendsftf2;external;
{$L x64/extendsftf2.o}
procedure __extenddftf2;external;
{$L x64/extenddftf2.o}
procedure __extendxftf2;external;
{$L x64/extendxftf2.o}
procedure __trunctfsf2;external;
{$L x64/trunctfsf2.o}
procedure __trunctfdf2;external;
{$L x64/trunctfdf2.o}
procedure __trunctfxf2;external;
{$L x64/trunctfxf2.o}
procedure __enable_execute_stack;external;
{$L x64/enable-execute-stack.o}
procedure &abort;cdecl;
function atexit(p:Pointer):integer;cdecl;
{$ENDIF}
{$IFDEF WIN32}
procedure ___chkstk;external;
{$L x86/_chkstk.o}
procedure __alloca;external;
{$L x86/_chkstk.o}
procedure ___chkstk_ms;external;
{$L x86/_chkstk_ms.o}
procedure ___muldi3;external;
{$L x86/_muldi3.o}
procedure ___negdi2;external;
{$L x86/_negdi2.o}
procedure ___lshrdi3;external;
{$L x86/_lshrdi3.o}
procedure ___ashldi3;external;
{$L x86/_ashldi3.o}
procedure ___ashrdi3;external;
{$L x86/_ashrdi3.o}
procedure ___cmpdi2;external;
{$L x86/_cmpdi2.o}
procedure ___ucmpdi2;external;
{$L x86/_ucmpdi2.o}
procedure ___clear_cache;external;
{$L x86/_clear_cache.o}
procedure ___do_global_dtors;external;
{$L x86/__main.o}
procedure ___absvsi2;external;
{$L x86/_absvsi2.o}
procedure ___absvdi2;external;
{$L x86/_absvdi2.o}
procedure ___addvsi3;external;
{$L x86/_addvsi3.o}
procedure ___addvdi3;external;
{$L x86/_addvdi3.o}
procedure ___subvsi3;external;
{$L x86/_subvsi3.o}
procedure ___subvdi3;external;
{$L x86/_subvdi3.o}
procedure ___mulvsi3;external;
{$L x86/_mulvsi3.o}
procedure ___mulvdi3;external;
{$L x86/_mulvdi3.o}
procedure ___negvsi2;external;
{$L x86/_negvsi2.o}
procedure ___negvdi2;external;
{$L x86/_negvdi2.o}
procedure ___ffssi2;external;
{$L x86/_ffssi2.o}
procedure ___ffsdi2;external;
{$L x86/_ffsdi2.o}
procedure ___clzsi2;external;
{$L x86/_clzsi2.o}
procedure ___clzdi2;external;
{$L x86/_clzdi2.o}
procedure ___ctzsi2;external;
{$L x86/_ctzsi2.o}
procedure ___ctzdi2;external;
{$L x86/_ctzdi2.o}
procedure ___popcountsi2;external;
{$L x86/_popcountsi2.o}
procedure ___popcountdi2;external;
{$L x86/_popcountdi2.o}
procedure ___paritysi2;external;
{$L x86/_paritysi2.o}
procedure ___paritydi2;external;
{$L x86/_paritydi2.o}
procedure ___powisf2;external;
{$L x86/_powisf2.o}
procedure ___powidf2;external;
{$L x86/_powidf2.o}
procedure ___powixf2;external;
{$L x86/_powixf2.o}
procedure ___powitf2;external;
{$L x86/_powitf2.o}
procedure ___mulsc3;external;
{$L x86/_mulsc3.o}
procedure ___muldc3;external;
{$L x86/_muldc3.o}
procedure ___mulxc3;external;
{$L x86/_mulxc3.o}
procedure ___multc3;external;
{$L x86/_multc3.o}
procedure ___divsc3;external;
{$L x86/_divsc3.o}
procedure ___divdc3;external;
{$L x86/_divdc3.o}
procedure ___divxc3;external;
{$L x86/_divxc3.o}
procedure ___divtc3;external;
{$L x86/_divtc3.o}
procedure ___bswapsi2;external;
{$L x86/_bswapsi2.o}
procedure ___bswapdi2;external;
{$L x86/_bswapdi2.o}
procedure ___clrsbsi2;external;
{$L x86/_clrsbsi2.o}
procedure ___clrsbdi2;external;
{$L x86/_clrsbdi2.o}
procedure ___fixunssfsi;external;
{$L x86/_fixunssfsi.o}
procedure ___fixunsdfsi;external;
{$L x86/_fixunsdfsi.o}
procedure ___fixunsxfsi;external;
{$L x86/_fixunsxfsi.o}
procedure ___fixsfdi;external;
{$L x86/_fixsfdi.o}
procedure ___fixdfdi;external;
{$L x86/_fixdfdi.o}
procedure ___fixxfdi;external;
{$L x86/_fixxfdi.o}
procedure ___fixunssfdi;external;
{$L x86/_fixunssfdi.o}
procedure ___fixunsdfdi;external;
{$L x86/_fixunsdfdi.o}
procedure ___fixunsxfdi;external;
{$L x86/_fixunsxfdi.o}
procedure ___floatdisf;external;
{$L x86/_floatdisf.o}
procedure ___floatdidf;external;
{$L x86/_floatdidf.o}
procedure ___floatdixf;external;
{$L x86/_floatdixf.o}
procedure ___floatundisf;external;
{$L x86/_floatundisf.o}
procedure ___floatundidf;external;
{$L x86/_floatundidf.o}
procedure ___floatundixf;external;
{$L x86/_floatundixf.o}
procedure ___gcc_bcmp;external;
{$L x86/__gcc_bcmp.o}
procedure ___divdi3;external;
{$L x86/_divdi3.o}
procedure ___moddi3;external;
{$L x86/_moddi3.o}
procedure ___divmoddi4;external;
{$L x86/_divmoddi4.o}
procedure ___udivdi3;external;
{$L x86/_udivdi3.o}
procedure ___umoddi3;external;
{$L x86/_umoddi3.o}
procedure ___udivmoddi4;external;
{$L x86/_udivmoddi4.o}
procedure ___udiv_w_sdiv;external;
{$L x86/_udiv_w_sdiv.o}
procedure ___dfp_set_round;external;
{$L x86/bid_decimal_globals.o}
procedure ___bid32_to_binary32;external;
{$L x86/bid_binarydecimal.o}
procedure _isinfd32;external;
{$L x86/_isinfd32.o}
procedure _isinfd64;external;
{$L x86/_isinfd64.o}
procedure _isinfd128;external;
{$L x86/_isinfd128.o}
procedure ___bid64_isSigned;external;
{$L x86/bid64_noncomp.o}
procedure ___bid128_isSigned;external;
{$L x86/bid128_noncomp.o}
procedure ___bid_round64_2_18;external;
{$L x86/bid_round.o}
procedure ___bid64_from_int32;external;
{$L x86/bid_from_int.o}
procedure ___bid64_add;external;
{$L x86/bid64_add.o}
procedure ___bid64dq_add;external;
{$L x86/bid128_add.o}
procedure ___bid64_mul;external;
{$L x86/bid64_mul.o}
procedure ___bid64qq_mul;external;
{$L x86/bid128_mul.o}
procedure ___bid64_quiet_equal;external;
{$L x86/bid64_compare.o}
procedure ___bid128_quiet_equal;external;
{$L x86/bid128_compare.o}
procedure ___bid32_to_bid64;external;
{$L x86/bid32_to_bid64.o}
procedure ___bid32_to_bid128;external;
{$L x86/bid32_to_bid128.o}
procedure ___bid64_to_bid128;external;
{$L x86/bid64_to_bid128.o}
procedure ___bid64_to_int32_rnint;external;
{$L x86/bid64_to_int32.o}
procedure ___bid64_to_int64_rnint;external;
{$L x86/bid64_to_int64.o}
procedure ___bid64_to_uint32_rnint;external;
{$L x86/bid64_to_uint32.o}
procedure ___bid64_to_uint64_rnint;external;
{$L x86/bid64_to_uint64.o}
procedure ___bid128_to_int32_rnint;external;
{$L x86/bid128_to_int32.o}
procedure ___bid128_to_int64_rnint;external;
{$L x86/bid128_to_int64.o}
procedure ___bid128_to_uint32_rnint;external;
{$L x86/bid128_to_uint32.o}
procedure ___bid128_to_uint64_rnint;external;
{$L x86/bid128_to_uint64.o}
procedure ___bid_addsd3;external;
{$L x86/_addsub_sd.o}
procedure ___bid_divsd3;external;
{$L x86/_div_sd.o}
procedure ___bid_mulsd3;external;
{$L x86/_mul_sd.o}
procedure ___bid_eqsd2;external;
{$L x86/_eq_sd.o}
procedure ___bid_nesd2;external;
{$L x86/_ne_sd.o}
procedure ___bid_ltsd2;external;
{$L x86/_lt_sd.o}
procedure ___bid_gtsd2;external;
{$L x86/_gt_sd.o}
procedure ___bid_lesd2;external;
{$L x86/_le_sd.o}
procedure ___bid_gesd2;external;
{$L x86/_ge_sd.o}
procedure ___bid_fixsdsi;external;
{$L x86/_sd_to_si.o}
procedure ___bid_fixsddi;external;
{$L x86/_sd_to_di.o}
procedure ___bid_fixunssdsi;external;
{$L x86/_sd_to_usi.o}
procedure ___bid_fixunssddi;external;
{$L x86/_sd_to_udi.o}
procedure ___bid_floatsisd;external;
{$L x86/_si_to_sd.o}
procedure ___bid_floatdisd;external;
{$L x86/_di_to_sd.o}
procedure ___bid_floatunssisd;external;
{$L x86/_usi_to_sd.o}
procedure ___bid_floatunsdisd;external;
{$L x86/_udi_to_sd.o}
procedure ___bid_truncsdsf;external;
{$L x86/_sd_to_sf.o}
procedure ___bid_extendsddf;external;
{$L x86/_sd_to_df.o}
procedure ___bid_extendsdxf;external;
{$L x86/_sd_to_xf.o}
procedure ___bid_extendsdtf;external;
{$L x86/_sd_to_tf.o}
procedure ___bid_extendsfsd;external;
{$L x86/_sf_to_sd.o}
procedure ___bid_truncdfsd;external;
{$L x86/_df_to_sd.o}
procedure ___bid_truncxfsd;external;
{$L x86/_xf_to_sd.o}
procedure ___bid_trunctfsd;external;
{$L x86/_tf_to_sd.o}
procedure ___bid_extendsddd2;external;
{$L x86/_sd_to_dd.o}
procedure ___bid_extendsdtd2;external;
{$L x86/_sd_to_td.o}
procedure ___bid_unordsd2;external;
{$L x86/_unord_sd.o}
procedure ___bid_adddd3;external;
{$L x86/_addsub_dd.o}
procedure ___bid_divdd3;external;
{$L x86/_div_dd.o}
procedure ___bid_muldd3;external;
{$L x86/_mul_dd.o}
procedure ___bid_eqdd2;external;
{$L x86/_eq_dd.o}
procedure ___bid_nedd2;external;
{$L x86/_ne_dd.o}
procedure ___bid_ltdd2;external;
{$L x86/_lt_dd.o}
procedure ___bid_gtdd2;external;
{$L x86/_gt_dd.o}
procedure ___bid_ledd2;external;
{$L x86/_le_dd.o}
procedure ___bid_gedd2;external;
{$L x86/_ge_dd.o}
procedure ___bid_fixddsi;external;
{$L x86/_dd_to_si.o}
procedure ___bid_fixdddi;external;
{$L x86/_dd_to_di.o}
procedure ___bid_fixunsddsi;external;
{$L x86/_dd_to_usi.o}
procedure ___bid_fixunsdddi;external;
{$L x86/_dd_to_udi.o}
procedure ___bid_floatsidd;external;
{$L x86/_si_to_dd.o}
procedure ___bid_floatdidd;external;
{$L x86/_di_to_dd.o}
procedure ___bid_floatunssidd;external;
{$L x86/_usi_to_dd.o}
procedure ___bid_floatunsdidd;external;
{$L x86/_udi_to_dd.o}
procedure ___bid_truncddsf;external;
{$L x86/_dd_to_sf.o}
procedure ___bid_truncdddf;external;
{$L x86/_dd_to_df.o}
procedure ___bid_extendddxf;external;
{$L x86/_dd_to_xf.o}
procedure ___bid_extendddtf;external;
{$L x86/_dd_to_tf.o}
procedure ___bid_extendsfdd;external;
{$L x86/_sf_to_dd.o}
procedure ___bid_extenddfdd;external;
{$L x86/_df_to_dd.o}
procedure ___bid_truncxfdd;external;
{$L x86/_xf_to_dd.o}
procedure ___bid_trunctfdd;external;
{$L x86/_tf_to_dd.o}
procedure ___bid_truncddsd2;external;
{$L x86/_dd_to_sd.o}
procedure ___bid_extendddtd2;external;
{$L x86/_dd_to_td.o}
procedure ___bid_unorddd2;external;
{$L x86/_unord_dd.o}
procedure ___bid_addtd3;external;
{$L x86/_addsub_td.o}
procedure ___bid_divtd3;external;
{$L x86/_div_td.o}
procedure ___bid_multd3;external;
{$L x86/_mul_td.o}
procedure ___bid_eqtd2;external;
{$L x86/_eq_td.o}
procedure ___bid_netd2;external;
{$L x86/_ne_td.o}
procedure ___bid_lttd2;external;
{$L x86/_lt_td.o}
procedure ___bid_gttd2;external;
{$L x86/_gt_td.o}
procedure ___bid_letd2;external;
{$L x86/_le_td.o}
procedure ___bid_getd2;external;
{$L x86/_ge_td.o}
procedure ___bid_fixtdsi;external;
{$L x86/_td_to_si.o}
procedure ___bid_fixtddi;external;
{$L x86/_td_to_di.o}
procedure ___bid_fixunstdsi;external;
{$L x86/_td_to_usi.o}
procedure ___bid_fixunstddi;external;
{$L x86/_td_to_udi.o}
procedure ___bid_floatsitd;external;
{$L x86/_si_to_td.o}
procedure ___bid_floatditd;external;
{$L x86/_di_to_td.o}
procedure ___bid_floatunssitd;external;
{$L x86/_usi_to_td.o}
procedure ___bid_floatunsditd;external;
{$L x86/_udi_to_td.o}
procedure ___bid_trunctdsf;external;
{$L x86/_td_to_sf.o}
procedure ___bid_trunctddf;external;
{$L x86/_td_to_df.o}
procedure ___bid_trunctdxf;external;
{$L x86/_td_to_xf.o}
procedure ___bid_trunctdtf;external;
{$L x86/_td_to_tf.o}
procedure ___bid_extendsftd;external;
{$L x86/_sf_to_td.o}
procedure ___bid_extenddftd;external;
{$L x86/_df_to_td.o}
procedure ___bid_extendxftd;external;
{$L x86/_xf_to_td.o}
procedure ___bid_extendtftd;external;
{$L x86/_tf_to_td.o}
procedure ___bid_trunctdsd2;external;
{$L x86/_td_to_sd.o}
procedure ___bid_trunctddd2;external;
{$L x86/_td_to_dd.o}
procedure ___bid_unordtd2;external;
{$L x86/_unord_td.o}
procedure ___copysigntf3;external;
{$L x86/tf-signs.o}
procedure ___sfp_handle_exceptions;external;
{$L x86/sfp-exceptions.o}
procedure ___addtf3;external;
{$L x86/addtf3.o}
procedure ___divtf3;external;
{$L x86/divtf3.o}
procedure ___eqtf2;external;
{$L x86/eqtf2.o}
procedure ___netf2;external;
{$L x86/eqtf2.o}
procedure ___getf2;external;
{$L x86/getf2.o}
procedure ___gttf2;external;
{$L x86/getf2.o}
procedure ___letf2;external;
{$L x86/letf2.o}
procedure ___lttf2;external;
{$L x86/letf2.o}
procedure ___multf3;external;
{$L x86/multf3.o}
procedure ___negtf2;external;
{$L x86/negtf2.o}
procedure ___subtf3;external;
{$L x86/subtf3.o}
procedure ___unordtf2;external;
{$L x86/unordtf2.o}
procedure ___fixtfsi;external;
{$L x86/fixtfsi.o}
procedure ___fixunstfsi;external;
{$L x86/fixunstfsi.o}
procedure ___floatsitf;external;
{$L x86/floatsitf.o}
procedure ___floatunsitf;external;
{$L x86/floatunsitf.o}
procedure ___fixtfdi;external;
{$L x86/fixtfdi.o}
procedure ___fixunstfdi;external;
{$L x86/fixunstfdi.o}
procedure ___floatditf;external;
{$L x86/floatditf.o}
procedure ___floatunditf;external;
{$L x86/floatunditf.o}
procedure ___extendsftf2;external;
{$L x86/extendsftf2.o}
procedure ___extenddftf2;external;
{$L x86/extenddftf2.o}
procedure ___extendxftf2;external;
{$L x86/extendxftf2.o}
procedure ___trunctfsf2;external;
{$L x86/trunctfsf2.o}
procedure ___trunctfdf2;external;
{$L x86/trunctfdf2.o}
procedure ___trunctfxf2;external;
{$L x86/trunctfxf2.o}
procedure _abort;cdecl;
function _atexit(p:Pointer):integer;cdecl;
{$ENDIF}
{$IFDEF WIN64}
var
  __imp_VirtualProtect,
  __DTOR_LIST__,__CTOR_LIST__ : UInt64;
  __bid_char_table2,
  __bid_char_table3,
  __bid_convert_table,
  __bid_estimate_bin_expon,
  __bid_estimate_decimal_digits,
  __bid_Ex128m128,
  __bid_Ex192m192,
  __bid_Ex256m256,
  __bid_Ex64m64,
  __bid_factors,
  __bid_half128,
  __bid_half192,
  __bid_half256,
  __bid_half64,
  __bid_IDEC_glbflags,
  __bid_IDEC_glbround,
  __bid_Kx128,
  __bid_Kx192,
  __bid_Kx256,
  __bid_Kx64,
  __bid_mask128,
  __bid_mask192,
  __bid_mask256,
  __bid_mask64,
  __bid_maskhigh128,
  __bid_maskhigh128M,
  __bid_maskhigh192M,
  __bid_maskhigh256M,
  __bid_midpoint128,
  __bid_midpoint192,
  __bid_midpoint256,
  __bid_midpoint64,
  __bid_nr_digits,
  __bid_onehalf128,
  __bid_onehalf128M,
  __bid_onehalf192M,
  __bid_onehalf256M,
  __bid_packed_10000_zeros,
  __bid_power10_index_binexp,
  __bid_power10_index_binexp_128,
  __bid_power10_table_128,
  __bid_reciprocals10_128,
  __bid_reciprocals10_64,
  __bid_recip_scale,
  __bid_round_const_table,
  __bid_round_const_table_128,
  __bid_shiftright128,
  __bid_shiftright128M,
  __bid_shiftright192M,
  __bid_shiftright256M,
  __bid_shift_ten2m3k128,
  __bid_shift_ten2m3k64,
  __bid_short_recip_scale,
  __bid_ten2k128,
  __bid_ten2k256,
  __bid_ten2k64,
  __bid_ten2m3k128,
  __bid_ten2m3k64,
  __bid_ten2mk128,
  __bid_ten2mk128M,
  __bid_ten2mk128trunc,
  __bid_ten2mk128truncM,
  __bid_ten2mk192M,
  __bid_ten2mk192truncM,
  __bid_ten2mk256M,
  __bid_ten2mk256truncM,
  __bid_ten2mk64,
  __bid_ten2mxtrunc128,
  __bid_ten2mxtrunc192,
  __bid_ten2mxtrunc256,
  __bid_ten2mxtrunc64,
  __clz_tab,
  __cpu_features,
  __cpu_model,
  __popcount_tab,
  __bid_round128_19_38,__bid64qqq_fma,__bid128_fma,__bid64_to_bid32,__bid64_sub,
  __bid64_div,__bid64_quiet_not_equal,__bid64_quiet_less,__bid64_quiet_greater,
  __bid64_quiet_less_equal,__bid64_quiet_greater_equal,__bid64_to_int32_xint,
  __bid64_to_int64_xint,__bid64_to_uint32_xint,__bid64_to_uint64_xint,
  __bid64_from_int64,__bid64_from_uint32,__bid64_from_uint64,__bid32_to_binary64,
  __bid32_to_binary80,__bid32_to_binary128,__binary32_to_bid32,
  __binary64_to_bid32,__binary80_to_bid32,__binary128_to_bid32,
  __bid64_quiet_unordered,__bid64_to_binary32,__bid64_to_binary64,
  __bid64_to_binary80,__bid64_to_binary128,__binary32_to_bid64,
  __binary64_to_bid64,__binary80_to_bid64,__binary128_to_bid64,
  __bid128_add,__bid128_sub,__bid128_div,__bid128_mul,__bid128_quiet_not_equal,
  __bid128_quiet_less,__bid128_quiet_greater,__bid128_quiet_less_equal,
  __bid128_quiet_greater_equal,__bid128_to_int32_xint,__bid128_to_int64_xint,
  __bid128_to_uint32_xint,__bid128_to_uint64_xint,__bid128_from_int32,
  __bid128_from_int64,__bid128_from_uint32,__bid128_from_uint64,
  __bid128_to_binary32,__bid128_to_binary64,__bid128_to_binary80,
  __bid128_to_binary128,__binary32_to_bid128,__binary64_to_bid128,
  __binary80_to_bid128,__binary128_to_bid128,__bid128_to_bid32,__bid128_to_bid64,
  __bid128_quiet_unordered,__imp_VirtualQuery
   :UInt64;
{$ENDIF}
{$IFDEF WIN32}
var
  ___DTOR_LIST__,___CTOR_LIST__ : UInt64;
  ___bid_char_table2,
  ___bid_char_table3,
  ___bid_convert_table,
  ___bid_estimate_bin_expon,
  ___bid_estimate_decimal_digits,
  ___bid_Ex128m128,
  ___bid_Ex192m192,
  ___bid_Ex256m256,
  ___bid_Ex64m64,
  ___bid_factors,
  ___bid_half128,
  ___bid_half192,
  ___bid_half256,
  ___bid_half64,
  ___bid_IDEC_glbflags,
  ___bid_IDEC_glbround,
  ___bid_Kx128,
  ___bid_Kx192,
  ___bid_Kx256,
  ___bid_Kx64,
  ___bid_mask128,
  ___bid_mask192,
  ___bid_mask256,
  ___bid_mask64,
  ___bid_maskhigh128,
  ___bid_maskhigh128M,
  ___bid_maskhigh192M,
  ___bid_maskhigh256M,
  ___bid_midpoint128,
  ___bid_midpoint192,
  ___bid_midpoint256,
  ___bid_midpoint64,
  ___bid_nr_digits,
  ___bid_onehalf128,
  ___bid_onehalf128M,
  ___bid_onehalf192M,
  ___bid_onehalf256M,
  ___bid_packed_10000_zeros,
  ___bid_power10_index_binexp,
  ___bid_power10_index_binexp_128,
  ___bid_power10_table_128,
  ___bid_reciprocals10_128,
  ___bid_reciprocals10_64,
  ___bid_recip_scale,
  ___bid_round_const_table,
  ___bid_round_const_table_128,
  ___bid_shiftright128,
  ___bid_shiftright128M,
  ___bid_shiftright192M,
  ___bid_shiftright256M,
  ___bid_shift_ten2m3k128,
  ___bid_shift_ten2m3k64,
  ___bid_short_recip_scale,
  ___bid_ten2k128,
  ___bid_ten2k256,
  ___bid_ten2k64,
  ___bid_ten2m3k128,
  ___bid_ten2m3k64,
  ___bid_ten2mk128,
  ___bid_ten2mk128M,
  ___bid_ten2mk128trunc,
  ___bid_ten2mk128truncM,
  ___bid_ten2mk192M,
  ___bid_ten2mk192truncM,
  ___bid_ten2mk256M,
  ___bid_ten2mk256truncM,
  ___bid_ten2mk64,
  ___bid_ten2mxtrunc128,
  ___bid_ten2mxtrunc192,
  ___bid_ten2mxtrunc256,
  ___bid_ten2mxtrunc64,
  ___clz_tab,
  ___cpu_features,
  ___cpu_model,
  ___popcount_tab,
  ___bid_round128_19_38,___bid64qqq_fma,___bid128_fma,___bid64_to_bid32,___bid64_sub,
  ___bid64_div,___bid64_quiet_not_equal,___bid64_quiet_less,___bid64_quiet_greater,
  ___bid64_quiet_less_equal,___bid64_quiet_greater_equal,___bid64_to_int32_xint,
  ___bid64_to_int64_xint,___bid64_to_uint32_xint,___bid64_to_uint64_xint,
  ___bid64_from_int64,___bid64_from_uint32,___bid64_from_uint64,___bid32_to_binary64,
  ___bid32_to_binary80,___bid32_to_binary128,___binary32_to_bid32,
  ___binary64_to_bid32,___binary80_to_bid32,___binary128_to_bid32,
  ___bid64_quiet_unordered,___bid64_to_binary32,___bid64_to_binary64,
  ___bid64_to_binary80,___bid64_to_binary128,___binary32_to_bid64,
  ___binary64_to_bid64,___binary80_to_bid64,___binary128_to_bid64,
  ___bid128_add,___bid128_sub,___bid128_div,___bid128_mul,___bid128_quiet_not_equal,
  ___bid128_quiet_less,___bid128_quiet_greater,___bid128_quiet_less_equal,
  ___bid128_quiet_greater_equal,___bid128_to_int32_xint,___bid128_to_int64_xint,
  ___bid128_to_uint32_xint,___bid128_to_uint64_xint,___bid128_from_int32,
  ___bid128_from_int64,___bid128_from_uint32,___bid128_from_uint64,
  ___bid128_to_binary32,___bid128_to_binary64,___bid128_to_binary80,
  ___bid128_to_binary128,___binary32_to_bid128,___binary64_to_bid128,
  ___binary80_to_bid128,___binary128_to_bid128,___bid128_to_bid32,___bid128_to_bid64,
  ___bid128_quiet_unordered,___imp_VirtualQuery
   :UInt64;
{$ENDIF}
implementation
{$IFDEF WIN64}
procedure &abort; cdecl;
begin
  abort;
end;

type TEXPROC = procedure; cdecl;
var EXPROC : TEXPROC;
function atexit(p:Pointer):integer;cdecl;
begin
  Result := 0;
  EXPROC := TEXPROC(P);
end;

{$ENDIF}
{$IFDEF WIN32}
uses sysutils;
procedure _abort; cdecl;
begin
  abort;
end;

type TEXPROC = procedure; cdecl;
var EXPROC : TEXPROC;
function _atexit(p:Pointer):integer;cdecl;
begin
  Result := 0;
  EXPROC := TEXPROC(P);
end;
{$ENDIF}
initialization
finalization
  if assigned(EXPROC) then EXPROC();
end.
