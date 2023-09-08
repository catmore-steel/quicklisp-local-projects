(in-package :rd-cost.web)

(defclass Constants ()
  ((PROJECT_NAME :initform "jq-rd-cost:")
   (TYPE_ENUM :initform "enum")
   (TYPE_CODE :initform "code")
   (RATE_SUM :initform "拟定占比必须总和为100!")
   (DEPT_ID_PERSONAL :initform 170)
   (UTF8 :initform "UTF-8")
   (GBK :initform "GBK")
   (HTTP :initform "http://")
   (HTTPS :initform "https://")
   (SUCCESS :initform "0")
   (FAIL :initform "1")
   (LOGIN_SUCCESS :initform "Success")
   (LOGOUT :initform "Logout")
   (LOGIN_FAIL :initform "Error")
   (CAPTCHA_CODE_KEY :initform "captcha_codes:") ;PROJECT_NAME + "captcha_codes:"
   (LOGIN_TOKEN_KEY :initform "login_tokens:") ;PROJECT_NAME + "login_tokens:"
   (REPEAT_SUBMIT_KEY :initform "repeat_submit:") ;PROJECT_NAME + "repeat_submit:"
   (SYS_REGION_KEY :initform "sys_region:") ;PROJECT_NAME + "sys_region:"
   (CAPTCHA_EXPIRATION :initform 2)
   (MESSAGE_CAPTCHA_EXPIRATION :initform 10)
   (TOKEN :initform "token")
   (TOKEN_PREFIX :initform "Bearer ")
   (LOGIN_USER_KEY :initform "login_user_key")
   (JWT_USERID :initform "userid")
   (JWT_USERNAME :initform "sub")
   (JWT_AVATAR :initform "avatar")
   (JWT_CREATED :initform "created")
   (JWT_AUTHORITIES :initform "authorities")
   (SYS_CONFIG_KEY :initform "sys_config:") ;PROJECT_NAME + "sys_config:"
   (SYS_DICT_KEY :initform "sys_dict:") ;PROJECT_NAME + "sys_dict:"
   (SYS_MENU_DATA_KEY :initform "sys_menu_data:") ;PROJECT_NAME + "sys_menu_data:"
   (RESOURCE_PREFIX :initform "/profile")
   (LOGIN_USERID_KEY :initform "login_userid:")
   (APPLY_COMPANY :initform "1")
   (AGENT_COMPANY :initform "2")
   (SEX_TYPE_MALE :initform "0")
   (SEX_TYPE_FEMALE :initform "1")
   (SEX_TYPE_UNKNOWN :initform "2")
   (DEL_FLAG_NOT :initform "0")
   (DEL_FLAG_YES :initform "1")
   (SYS_YES :initform "Y")
   (SYS_NO :initform "N")
   (COMMON_NUMBER_YES :initform 1)
   (COMMON_NUMBER_NO :initform 0)
   (COMMON_BOOLEAN_YES :initform t) ;true
   (COMMON_BOOLEAN_NO :initform nil) ; false
   (LOCAL :initform "1")
   (QINIU :initform "2")
   (APPLY_TYPE_NEW :initform "new")
   (APPLY_TYPE_REPLY :initform "reply")
   (APPLY_TYPE_REVIEW :initform "review")
   (APPLY_TYPE_INVALID :initform "invalid")
   (FEE_TYPE_APP :initform "app")
   (FEE_TYPE_ACTUA :initform "actua")
   (FEE_TYPE_REG :initform "reg")
  
   
   ))
