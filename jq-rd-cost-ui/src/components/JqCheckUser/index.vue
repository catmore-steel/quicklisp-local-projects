<!-- 级联选择器选择地址 -->
<template>
  <el-tooltip :disabled="isLoginUser" content="登录用户非当前处理人！" effect="dark" placement="top">
    <div style="float: left;">
      <el-form :disabled="!isLoginUser">
        <slot></slot>
      </el-form>
    </div>
  </el-tooltip>
</template>

<script>


import defaultSettings from "@/settings";

export default {
  name: "JqCheckUser",
  data() {
    return {
      isLoginUser: false,
      loginUserId: null,
    }
  },
  props: {
    userIds: {
      type: Array
    }
  },
  created() {
    this.checkUsers(this.userIds);
  },
  watch: {
    userIds(val) {
      this.checkUsers(val);
    }
  },
  methods: {
    checkUsers(userIds) {
      this.isLoginUser = false
      //当前登录人
      this.loginUserId  =   this.$store.state.user.userId
      //判断是否开发模式
      if (defaultSettings.isDebug) {
        this.isLoginUser = true
      } else if (userIds.indexOf(this.loginUserId) != -1) {
          this.isLoginUser = true
      }else{
        this.isLoginUser = false
      }
    }
  }
}
</script>

<style scoped>

</style>
