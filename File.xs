/* Win32API/File.xs */
#define PERL_NO_GET_CONTEXT
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
/*#include "patchlevel.h"*/

/* Uncomment the next line unless set "WRITE_PERL=>1" in Makefile.PL: */
#define NEED_newCONSTSUB
#include "ppport.h"

#ifdef WORD
# undef WORD
#endif

#define  WIN32_LEAN_AND_MEAN	/* Tell windows.h to skip much */
#include <windows.h>
#include <winioctl.h>

/*CONSTS_DEFINED*/

#ifndef INVALID_SET_FILE_POINTER
#   define INVALID_SET_FILE_POINTER	((DWORD)-1)
#endif

#define oDWORD DWORD

#if (PERL_REVISION <= 5 && PERL_VERSION < 5) || defined(__CYGWIN__)
# define win32_get_osfhandle _get_osfhandle
# ifdef __CYGWIN__
#  define win32_open_osfhandle(handle,mode) \
	(Perl_croak(aTHX_ "_open_osfhandle not implemented on Cygwin!"), -1)
# else
#  define win32_open_osfhandle _open_osfhandle
# endif
# ifdef _get_osfhandle
#  undef _get_osfhandle	/* stolen_get_osfhandle() isn't available here */
# endif
# ifdef _open_osfhandle
#  undef _open_osfhandle /* stolen_open_osfhandle() isn't available here */
# endif
#endif

#ifndef XST_mUV
# define XST_mUV(i,v)  (ST(i) = sv_2mortal(newSVuv(v))  )
#endif

#ifndef XSRETURN_UV
# define XSRETURN_UV(v) STMT_START { XST_mUV(0,v);  XSRETURN(1); } STMT_END
#endif

#ifndef DEBUGGING
# define	Debug(list)	/*Nothing*/
#else
# define	Debug(list)	ErrPrintf list
# include <stdarg.h>
    static void
    ErrPrintf( const char *sFmt, ... )
    {
      dTHX;
      va_list pAList;
      static char *sEnv= NULL;
      DWORD uErr= GetLastError();
	if(  NULL == sEnv  ) {
	    if(  NULL == ( sEnv= getenv("DEBUG_WIN32API_FILE") )  )
		sEnv= "";
	}
	if(  '\0' == *sEnv  )
	    return;
	va_start( pAList, sFmt );
	vfprintf( stderr, sFmt, pAList );
	va_end( pAList );
	SetLastError( uErr );
    }
#endif /* DEBUGGING */


#include "buffers.h"	/* Include this after DEBUGGING setup finished */

static LONG uLastFileErr= 0;

static void
SaveErr( BOOL bFailed )
{
    if(  bFailed  ) {
	uLastFileErr= GetLastError();
    }
}
/*do not turn null_arg into a function, compiler optimizes sv_flags
and sv_u SV head member dereferncing of null_arg and SvIPNUV together

each of these helpers was designed so that in the ideal case, no function
calls are made from the helper
*/
static BOOL BoolIn(pTHX_ SV * isvb){
    return null_arg(isvb)||!SvTRUE(isvb)
    ? (BOOL)0 :
        looks_like_number(isvb) ?
            (BOOL)SvIV(isvb) :
            (BOOL)1;    
}
static PVOID BufIn(pTHX_ SV * isvs){
    if(  null_arg(isvs)  )
	    return NULL;
	else
	    return (PVOID) SvPV_nolen( isvs );
}
static U_IVUV IntIn(pTHX_ SV * isvn, CBOOL bUV){
    U_IVUV ret;
    null_arg(isvn) ?
        ( bUV ?
            (ret.uv  = (UV)0)
            :(ret.iv  = (IV)0)
        ):( bUV ?
            (ret.uv = INT2PTR(UV,SvUV(isvn)))
            :(ret.iv = INT2PTR(IV,SvIV(isvn))));
    return ret;
}
static U_IVUV optNumIn(pTHX_ SV * isvn, CBOOL bUV){
    U_IVUV ret;
    if( null_arg(isvn) || !SvOK(isvn)){
        if(bUV)
            ret.uv  = (UV)0;
        else
            ret.iv  = (IV)0;
    }
    else{
        if(bUV)
            ret.uv = SvUV(isvn);
        else
            ret.iv = SvIV(isvn);
    }
    return ret;
}
/*an attempt to figure out what these macros do */
/*static DWORD fn_null_arg(pTHX_ SV * sv){
    return null_arg(sv);
}
static DWORD fn_autosize(pTHX_ SV * sv){
    return autosize(sv);
}
static DWORD fn_optUV(pTHX_ SV * sv){
    return optUV(sv);
}
static DWORD fn_init_buf_l(pTHX_ SV * svSize){
	return (  fn_null_arg(aTHX_ svSize) ? 0 : fn_autosize(aTHX_ svSize) ? fn_optUV(aTHX_ svSize)
	   : strtoul( 1+SvPV_nolen(svSize), NULL, 10 )  );
}
*/
/* Initialize a buffer size argument of type DWORD: */
static DWORD fn_init_buf_l(pTHX_ SV * svSize){
/*null_arg is the call($var1, [], $var2) array ref null thing */
    if(null_arg(svSize))
        return 0;
    else{
        char * s; /* expanded autosize macro with cached PV and ! removed*/
        if(  SvOK(svSize)  &&  ! SvROK(svSize)
			&&  (s = SvPV_nolen(svSize))
            &&  '=' == *s  ){
            return strtoul( 1+s, NULL, 10 );   
        }
        else{
            /* expanded optUV with cached null_arg and removed null_arg branch*/
            return !SvOK(svSize) ? 0 : (DWORD)SvUV(svSize);
        }
    }

}
/*original macro
#define init_buf_l( svSize )						\
	(  null_arg(svSize) ? 0 : autosize(svSize) ? optUV(svSize)	\
	   : strtoul( 1+SvPV_nolen(svSize), NULL, 10 )  )
*/
/* In INPUT section put "= init_buf_l($arg);" after variable name. */

static char * fn_grow_buf_l(pTHX_ SV * svBuf, STRLEN * lSize, SV * svSize, CBOOL isWide ){
    char * sBuf;
	if(  null_arg(svBuf)  ) {
	    sBuf= NULL;
	} else {
        if(((SvFLAGS(svBuf) & SVf_OK) & ~ SVf_POK) ||SvTYPE(svBuf) < SVt_PV)
            sv_setpvn(svBuf,"",0);
        SvCUR(svBuf) = 0;
        /*combination of lwSvGROW and lSvGROW*/
        {   const STRLEN bufLen = (isWide ? sizeof(WCHAR) : 1)*( 0==(*lSize) ? MIN_GROW_SIZE : (*lSize)+1 );
            sBuf = SvGROW(svBuf, bufLen);
        }
        *(wchar_t *)sBuf = L'\0'; /*technical overflow here*/
        {
            char * s;
            //expanded autosize macro
            if(!(SvOK(svSize)  &&  ! SvROK(svSize)
                &&  ((s = SvPV_nolen(svSize)), ('=' == *s))))
                  *lSize=(isWide ? SvLEN(svBuf) /sizeof(WCHAR): SvLEN(svBuf)) - 1;
        }
	}
    return sBuf;
}

/*flags for NumBufIn*/
#define NUMBUFIN_UV 0
#define NUMBUFIN_IV 1
#define NUMBUFIN_OPT 0 /*dont warn on undef*/
#define NUMBUFIN_REQ 2 /*warn on undef*/

static U_IVUV * NumBufIn(pTHX_ SV * sv, U_IVUV * pn, UCHAR flags){
    U_IVUV * ret;
	if(  null_arg(sv)  )
	    ret = NULL;
    else{
        ret = pn;
        if(flags & NUMBUFIN_REQ){
            getnum:
            if(flags & NUMBUFIN_IV) ret->iv = SvIV(sv);
            else                    ret->uv = SvUV(sv);
        }
        else{ /*optional branch, dont warn on undef*/
            if(SvOK(sv)) goto getnum;
            else{
                if(flags & NUMBUFIN_IV) ret->iv  = (IV)0;
                else                    ret->uv  = (UV)0;
            }
        }
    }
    return ret;
}

static void BoolOut(pTHX_ SV * arg, BOOL var){
	if(  ! null_arg(arg)  &&  ! SvREADONLY(arg)  ) {
	    if(  var  ) {
		sv_setiv( arg, (IV)var );
	    } else {
		sv_setsv( arg, &PL_sv_no );
	    }
	}
}

/*flags for NumOut*/
#define NUMOUT_UV 0
#define NUMOUT_IV 1
#define NUMOUT_OUT 0 /*fatally error in sv_set*v if SvREADONLY*/
#define NUMOUT_IN 2 /*do not set the SV if it is SvREADONLY (numeric constant in Perl Lang)*/
#define NUMOUT_LIT 0 /*UPU_IVUV use member "num " for U_IVUV  */
#define NUMOUT_PTR 4 /*UPU_IVUV member "p" should be dereferenced to get U_IVUV*/
#define NUMOUT_NOMAGIC 0 /*do not do SvSETMAGIC on SV * */
#define NUMOUT_MAGIC 8 /*do SvSETMAGIC on SV * */
/*next 2 size flags are meaningless if not NUMOUT_PTR*/
#define NUMOUT_4BYTES 0 /*p is a pointer to 32 bit number, I/UV flag important too*/
#define NUMOUT_8BYTES 16 /*p is a pointer to 64 bit number, I/UV flag important too
                          , will truncate on 32 bit IV perl*/
static void NumOut(pTHX_ SV * arg, UPU_IVUV var, UCHAR flags){
    if(  ! null_arg(arg)  &&  ((flags & NUMOUT_IN) ? ! SvREADONLY(arg) : 1)  ){
        if(flags & NUMOUT_PTR)
	{
	  if(flags & NUMOUT_8BYTES){
	    if(flags & NUMOUT_IV) var.num.iv = (IV) *(var.p.i64);
	    else var.num.uv = (UV) *(var.p.u64);
	  } else {
	    if(flags & NUMOUT_IV) var.num.iv = (IV) *(var.p.i32);
	    else var.num.uv = (UV) *(var.p.u32);
	  }
	}
        if(flags & NUMOUT_IV)
            sv_setiv(arg, var.num.iv);
        else
            sv_setuv(arg, var.num.uv);
        if(flags & NUMOUT_MAGIC) SvSETMAGIC(arg);
    }
}

MODULE = Win32API::File		PACKAGE = Win32API::File

PROTOTYPES: DISABLE


LONG
_fileLastError( uError=0 )
	DWORD	uError
    CODE:
	if(  1 <= items  ) {
	    uLastFileErr= uError;
	}
	RETVAL= uLastFileErr;
    OUTPUT:
	RETVAL


BOOL
CloseHandle( hObject )
	HANDLE	hObject
    CODE:
        RETVAL = CloseHandle( hObject );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL


BOOL
CopyFileA( sOldFileName, sNewFileName, bFailIfExists )
	char *	sOldFileName
	char *	sNewFileName
	BOOL	bFailIfExists
    CODE:
        RETVAL = CopyFileA( sOldFileName, sNewFileName, bFailIfExists );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL


BOOL
CopyFileW( swOldFileName, swNewFileName, bFailIfExists )
	WCHAR *	swOldFileName
	WCHAR *	swNewFileName
	BOOL	bFailIfExists
    CODE:
        RETVAL = CopyFileW( swOldFileName, swNewFileName, bFailIfExists );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL


void
CreateFileA( sPath, uAccess, uShare, pSecAttr, uCreate, uFlags, hModel )
	char *	sPath
	DWORD	uAccess
	DWORD	uShare
	void *	pSecAttr
	DWORD	uCreate
	DWORD	uFlags
	HANDLE	hModel
    PREINIT:
    HANDLE hreturn;
    SV *  svreturn;
    PPCODE:
	hreturn= CreateFileA( sPath, uAccess, uShare,
	  pSecAttr, uCreate, uFlags, hModel );
	if(  INVALID_HANDLE_VALUE == hreturn  ) {
	    SaveErr( 1 );
        svreturn = &PL_sv_no;
	} else {
        if(  0 == hreturn  ) {
            svreturn = newSVpvs("0 but true" );
        } else {
            svreturn = newSVuv(PTR2UV(hreturn));
        }
        sv_2mortal(svreturn);
	}
    PUSHs(svreturn);


void
CreateFileW( swPath, uAccess, uShare, pSecAttr, uCreate, uFlags, hModel )
	WCHAR *	swPath
	DWORD	uAccess
	DWORD	uShare
	void *	pSecAttr
	DWORD	uCreate
	DWORD	uFlags
	HANDLE	hModel
    PREINIT:
    HANDLE hreturn;
    SV *  svreturn;
    PPCODE:
	hreturn= CreateFileW( swPath, uAccess, uShare,
	  pSecAttr, uCreate, uFlags, hModel );
	if(  INVALID_HANDLE_VALUE == hreturn  ) {
	    SaveErr( 1 );
        svreturn = &PL_sv_no;
	} else {
        if(  0 == hreturn  ) {
            svreturn = newSVpvs("0 but true" );
        } else {
            svreturn = newSVuv(PTR2UV(hreturn));
        }
        sv_2mortal(svreturn);
	}
    PUSHs(svreturn);


BOOL
DefineDosDeviceA( uFlags, sDosDeviceName, sTargetPath )
	DWORD	uFlags
	char *	sDosDeviceName
	char *	sTargetPath
    CODE:
        RETVAL = DefineDosDeviceA( uFlags, sDosDeviceName, sTargetPath );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL


BOOL
DefineDosDeviceW( uFlags, swDosDeviceName, swTargetPath )
	DWORD	uFlags
	WCHAR *	swDosDeviceName
	WCHAR *	swTargetPath
    CODE:
        RETVAL = DefineDosDeviceW( uFlags, swDosDeviceName, swTargetPath );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL


BOOL
DeleteFileA( sFileName )
	char *	sFileName
    CODE:
        RETVAL = DeleteFileA( sFileName );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL


BOOL
DeleteFileW( swFileName )
	WCHAR *	swFileName
    CODE:
        RETVAL = DeleteFileW( swFileName );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL

# todo
BOOL
DeviceIoControl( hDevice, uIoControlCode, pInBuf, lInBuf, opOutBuf, lOutBuf, olRetBytes, pOverlapped )
	HANDLE	hDevice
	DWORD	uIoControlCode
	char *	pInBuf
	DWORD	lInBuf		= init_buf_l($arg);
	char *	opOutBuf	= NO_INIT
	DWORD	lOutBuf		= init_buf_l($arg);
	oDWORD	&olRetBytes
	void *	pOverlapped
    CODE:
	if(  NULL != pInBuf  ) {
	    if(  0 == lInBuf  ) {
		lInBuf= SvCUR(ST(2));
	    } else if(  SvCUR(ST(2)) < lInBuf  ) {
		croak( "%s: pInBuf shorter than specified (%d < %d)",
		  "Win32API::File::DeviceIoControl", SvCUR(ST(2)), lInBuf );
	    }
	}
	grow_buf_l( opOutBuf,ST(4),char *, lOutBuf,ST(5) );
	RETVAL= DeviceIoControl( hDevice, uIoControlCode, pInBuf, lInBuf,
		  opOutBuf, lOutBuf, &olRetBytes, pOverlapped );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL
	opOutBuf	trunc_buf_l( RETVAL, opOutBuf,ST(4), olRetBytes );
	olRetBytes


HANDLE
FdGetOsFHandle( ivFd )
	int	ivFd
    CODE:
	RETVAL= (HANDLE) win32_get_osfhandle( ivFd );
	SaveErr( INVALID_HANDLE_VALUE == RETVAL );
    OUTPUT:
	RETVAL


DWORD
GetDriveTypeA( sRootPath )
	char *	sRootPath
    CODE:
        RETVAL = GetDriveTypeA( sRootPath );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL


DWORD
GetDriveTypeW( swRootPath )
	WCHAR *	swRootPath
    CODE:
        RETVAL = GetDriveTypeW( swRootPath );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL


DWORD
GetFileAttributesA( sPath )
	char *	sPath
    CODE:
        RETVAL = GetFileAttributesA( sPath );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL


DWORD
GetFileAttributesW( swPath )
	WCHAR *	swPath
    CODE:
        RETVAL = GetFileAttributesW( swPath );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL


DWORD
GetFileType( hFile )
	HANDLE	hFile
    CODE:
        RETVAL = GetFileType( hFile );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL


BOOL
GetHandleInformation( hObject, ouFlags )
	HANDLE		hObject
	oDWORD *	ouFlags
    CODE:
        RETVAL = GetHandleInformation( hObject, ouFlags );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL
	ouFlags


DWORD
GetLogicalDrives()
    CODE:
        RETVAL = GetLogicalDrives();
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL


DWORD
GetLogicalDriveStringsA( lBufSize, osBuffer )
    PREINIT:
    SV * svosBuffer = ST(1);/*flipped for locality*/
    SV * svlBufSize = ST(0);
    INPUT:
	DWORD	lBufSize	= init_buf_l(svlBufSize);
	char *	osBuffer	= NO_INIT
    CODE:
	grow_buf_l( osBuffer,svosBuffer,char *, lBufSize,svlBufSize );
	RETVAL= GetLogicalDriveStringsA( lBufSize, osBuffer );
	if(  lBufSize < RETVAL  &&  autosize(svlBufSize)  ) {
	    lBufSize= RETVAL;
	    grow_buf_l( osBuffer,svosBuffer,char *, lBufSize,svlBufSize );
	    RETVAL= GetLogicalDriveStringsA( lBufSize, osBuffer );
	}
	if(  0 == RETVAL  ||  lBufSize < RETVAL  ) {
	    SaveErr( 1 );
	} else {
	    trunc_buf_l( 1, osBuffer,svosBuffer, RETVAL );
	}
    OUTPUT:
	RETVAL
	osBuffer	;/* The code for this appears above. */


DWORD
GetLogicalDriveStringsW( lwBufSize, oswBuffer )
    PREINIT:
    SV * svoswBuffer = ST(1);/*flipped for locality*/
    SV * svlwBufSize = ST(0);
    INPUT:
	DWORD	lwBufSize	= init_buf_lw(svlwBufSize);
	WCHAR *	oswBuffer	= NO_INIT
    CODE:
	grow_buf_lw( oswBuffer,svoswBuffer, lwBufSize,svlwBufSize );
	RETVAL= GetLogicalDriveStringsW( lwBufSize, oswBuffer );
	if(  lwBufSize < RETVAL  &&  autosize(svlwBufSize)  ) {
	    lwBufSize= RETVAL;
	    grow_buf_lw( oswBuffer,svoswBuffer, lwBufSize,svlwBufSize );
	    RETVAL= GetLogicalDriveStringsW( lwBufSize, oswBuffer );
	}
	if(  0 == RETVAL  ||  lwBufSize < RETVAL  ) {
	    SaveErr( 1 );
	} else {
	    trunc_buf_lw( 1, oswBuffer,svoswBuffer, RETVAL );
	}
    OUTPUT:
	RETVAL
	oswBuffer	;/* The code for this appears above. */


BOOL
GetVolumeInformationA( sRootPath, osVolName, lVolName, ouSerialNum, ouMaxNameLen, ouFsFlags, osFsType, lFsType )
    PREINIT:
    SV * svosVolName;
    SV * svlVolName = ST(2);
    SV * svosFsType;
    SV * svlFsType;
    INPUT:   
	char *	sRootPath
	char *	osVolName	= NO_INIT
	DWORD	lVolName	= init_buf_l(svlVolName);
	oDWORD	ouSerialNum	
	oDWORD	ouMaxNameLen
	oDWORD	ouFsFlags
	char *	osFsType	= NO_INIT
	DWORD	lFsType		= init_buf_l( svlFsType  = $arg);
    CODE:
    svosVolName = ST(1);
	grow_buf_l( osVolName,svosVolName,char *, lVolName,svlVolName );
    svosFsType = ST(6);
	grow_buf_l( osFsType,svosFsType,char *, lFsType,svlFsType );
	RETVAL= GetVolumeInformationA( sRootPath, osVolName, lVolName,
		  &ouSerialNum, &ouMaxNameLen, &ouFsFlags, osFsType, lFsType );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL
	osVolName	trunc_buf_z( RETVAL, osVolName,svosVolName );
	osFsType	trunc_buf_z( RETVAL, osFsType,svosFsType );
    SETMAGIC: DISABLE
	ouSerialNum ODWORDOUTMG(ouSerialNum);
	ouMaxNameLen ODWORDOUTMG(ouMaxNameLen);
	ouFsFlags ODWORDOUTMG(ouFsFlags);


BOOL
GetVolumeInformationW( swRootPath, oswVolName, lwVolName, ouSerialNum, ouMaxNameLen, ouFsFlags, oswFsType, lwFsType )
    PREINIT:
    SV * svoswVolName;
    SV * svlwVolName = ST(2);
    SV * svoswFsType;
    SV * svlwFsType;
    INPUT:
	WCHAR *	swRootPath
	WCHAR *	oswVolName	= NO_INIT
	DWORD	lwVolName	= init_buf_lw(svlwVolName);
	oDWORD	ouSerialNum	
	oDWORD	ouMaxNameLen
	oDWORD	ouFsFlags
	WCHAR *	oswFsType	= NO_INIT
	DWORD	lwFsType	= init_buf_lw( svlwFsType = $arg);
    CODE:
    svoswVolName = ST(1);
	grow_buf_lw( oswVolName,svoswVolName, lwVolName,svlwVolName );
    svoswFsType = ST(6);
	grow_buf_lw( oswFsType,svoswFsType, lwFsType,svlwFsType );
	RETVAL= GetVolumeInformationW( swRootPath, oswVolName, lwVolName,
	  &ouSerialNum, &ouMaxNameLen, &ouFsFlags, oswFsType, lwFsType );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL
	oswVolName	trunc_buf_zw( RETVAL, oswVolName,svoswVolName );
	oswFsType	trunc_buf_zw( RETVAL, oswFsType,svoswFsType );
    SETMAGIC: DISABLE
	ouSerialNum   ODWORDOUTMG(ouSerialNum)
	ouMaxNameLen ODWORDOUTMG(ouMaxNameLen);
	ouFsFlags ODWORDOUTMG(ouMaxNameLen);


BOOL
IsRecognizedPartition( ivPartitionType )
	int	ivPartitionType
    CODE:
        RETVAL = IsRecognizedPartition( ivPartitionType );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL


BOOL
IsContainerPartition( ivPartitionType )
	int	ivPartitionType
    CODE:
        RETVAL = IsContainerPartition( ivPartitionType );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL


BOOL
MoveFileA( sOldName, sNewName )
	char *	sOldName
	char *	sNewName
    CODE:
        RETVAL = MoveFileA( sOldName, sNewName );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL


BOOL
MoveFileW( swOldName, swNewName )
	WCHAR *	swOldName
	WCHAR *	swNewName
    CODE:
        RETVAL = MoveFileW( swOldName, swNewName );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL


BOOL
MoveFileExA( sOldName, sNewName, uFlags )
	char *	sOldName
	char *	sNewName
	DWORD	uFlags
    CODE:
        RETVAL = MoveFileExA( sOldName, sNewName, uFlags );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL


BOOL
MoveFileExW( swOldName, swNewName, uFlags )
	WCHAR *	swOldName
	WCHAR *	swNewName
	DWORD	uFlags
    CODE:
        RETVAL = MoveFileExW( swOldName, swNewName, uFlags );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL


SV *
OsFHandleOpenFd( hOsFHandle, uMode )
	long	hOsFHandle
	DWORD	uMode
    PREINIT:
    long    nrtn;
    CODE:
	nrtn= win32_open_osfhandle( hOsFHandle, uMode );
	if(  nrtn < 0  ) {
	    SaveErr( 1 );
        RETVAL = &PL_sv_no;
	} else {
        if(  0 == nrtn  ) {
            RETVAL = newSVpvs("0 but true" );
        } else {
            RETVAL = newSViv(PTR2IV(nrtn));
        }
	}
    OUTPUT:
    RETVAL

DWORD
QueryDosDeviceA( sDeviceName, osTargetPath, lTargetBuf )
    PREINIT:
    SV * svosTargetPath;
    SV * svlTargetBuf;
    INPUT:
	char *	sDeviceName
	char *	osTargetPath	= NO_INIT
	DWORD	lTargetBuf	= init_buf_l(svlTargetBuf = $arg);
    CODE:
    svosTargetPath = ST(1);
	grow_buf_l( osTargetPath,svosTargetPath,char *, lTargetBuf,svlTargetBuf );
	RETVAL= QueryDosDeviceA( sDeviceName, osTargetPath, lTargetBuf );
	SaveErr( 0 == RETVAL );
    OUTPUT:
	RETVAL
	osTargetPath	trunc_buf_l( 1, osTargetPath,svosTargetPath, RETVAL );


DWORD
QueryDosDeviceW( swDeviceName, oswTargetPath, lwTargetBuf )
    PREINIT:
    SV * svoswTargetPath;
    SV * svlwTargetBuf;
    INPUT:
	WCHAR *	swDeviceName
	WCHAR *	oswTargetPath	= NO_INIT
	DWORD	lwTargetBuf	= init_buf_lw( svlwTargetBuf = $arg);
    CODE:
    svoswTargetPath = ST(1);
	grow_buf_lw( oswTargetPath,svoswTargetPath, lwTargetBuf,svlwTargetBuf );
	RETVAL= QueryDosDeviceW( swDeviceName, oswTargetPath, lwTargetBuf );
	SaveErr( 0 == RETVAL );
    OUTPUT:
	RETVAL
	oswTargetPath	trunc_buf_lw( 1, oswTargetPath,svoswTargetPath, RETVAL );


BOOL
ReadFile( hFile, opBuffer, lBytes, olBytesRead, pOverlapped )
    PREINIT:
    SV * svopBuffer;
    SV * svlBytes;
    INPUT:
	HANDLE	hFile
	BYTE *	opBuffer	= NO_INIT
	DWORD	lBytes		= init_buf_l(svlBytes = $arg);
	oDWORD	&olBytesRead
	void *	pOverlapped
    CODE:
    svopBuffer = ST(1);
	grow_buf_l( opBuffer,svopBuffer,BYTE *, lBytes,svlBytes );
	/* Don't read more bytes than asked for if buffer is already big: */
	lBytes= init_buf_l(svlBytes);
	if(  0 == lBytes  &&  autosize(svlBytes)  ) {
	    lBytes= SvLEN( svopBuffer ) - 1;
	}
	RETVAL= ReadFile( hFile, opBuffer, lBytes, &olBytesRead, pOverlapped );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL
	opBuffer	trunc_buf_l( RETVAL, opBuffer,svopBuffer, olBytesRead );
    SETMAGIC: DISABLE
	olBytesRead ODWORDOUTMG(olBytesRead)


BOOL
GetOverlappedResult( hFile, lpOverlapped, lpNumberOfBytesTransferred, bWait)
	HANDLE hFile
	LPOVERLAPPED lpOverlapped
	LPDWORD lpNumberOfBytesTransferred
	BOOL bWait
    CODE:
    	RETVAL= GetOverlappedResult( hFile, lpOverlapped,
	 lpNumberOfBytesTransferred, bWait);
	SaveErr( !RETVAL );
    OUTPUT:
    	RETVAL
	lpOverlapped
	lpNumberOfBytesTransferred

DWORD
GetFileSize( hFile, lpFileSizeHigh )
	HANDLE hFile
	LPDWORD lpFileSizeHigh
    CODE:
    	RETVAL= GetFileSize( hFile, lpFileSizeHigh );
	SaveErr( NO_ERROR != GetLastError() );
    OUTPUT:
    	RETVAL
	lpFileSizeHigh

UINT
SetErrorMode( uNewMode )
	UINT	uNewMode


SV *
SetFilePointer( hFile, ivOffset, ioivOffsetHigh, uFromWhere )
	HANDLE	hFile
	LONG	ivOffset
	LONG *	ioivOffsetHigh
	DWORD	uFromWhere
    PREINIT:
    LONG nrtn;
    CODE:
	nrtn= SetFilePointer( hFile, ivOffset, ioivOffsetHigh, uFromWhere );
	if(  nrtn == INVALID_SET_FILE_POINTER && (GetLastError() != NO_ERROR)  ) {
	    SaveErr( 1 );
        RETVAL = &PL_sv_no;
	} else {
        if(  0 == nrtn  ) {
            RETVAL = newSVpvs("0 but true" );
        } else {
            RETVAL = newSViv(PTR2IV(nrtn));
        }
    }
    OUTPUT:
    RETVAL
	ioivOffsetHigh


BOOL
SetHandleInformation( hObject, uMask, uFlags )
	HANDLE	hObject
	DWORD	uMask
	DWORD	uFlags
    CODE:
        RETVAL = SetHandleInformation( hObject, uMask, uFlags );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL


BOOL
WriteFile( hFile, pBuffer, lBytes, ouBytesWritten, pOverlapped )
	HANDLE		hFile
	BYTE *		pBuffer
	DWORD		lBytes		= init_buf_l($arg);
	oDWORD	&ouBytesWritten
	void *		pOverlapped
    CODE:
	/* SvCUR(ST(1)) might "panic" if pBuffer isn't valid */
	if(  0 == lBytes  ) {
	    lBytes= SvCUR(svpBuffer);
	} else if(  SvCUR(svpBuffer) < lBytes  ) {
	    croak( "%s: pBuffer value too short (%d < %d)",
	      "Win32API::File::WriteFile", SvCUR(svpBuffer), lBytes );
	}
	RETVAL= WriteFile( hFile, pBuffer, lBytes,
		  &ouBytesWritten, pOverlapped );
	SaveErr( !RETVAL );
    OUTPUT:
	RETVAL
    SETMAGIC: DISABLE
	ouBytesWritten ODWORDOUTMG(ouBytesWritten)

void
GetStdHandle(fd)
    DWORD fd
PPCODE:
#ifdef _WIN64
    XSRETURN_IV((DWORD_PTR)GetStdHandle(fd));
#else
    XSRETURN_IV((DWORD)GetStdHandle(fd));
#endif

void
SetStdHandle(fd,handle)
    DWORD fd
    HANDLE handle
PPCODE:
    if (SetStdHandle(fd, handle))
	XSRETURN_YES;
    else
	XSRETURN_NO;
