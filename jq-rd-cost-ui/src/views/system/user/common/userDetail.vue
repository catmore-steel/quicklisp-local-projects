<template>
  <div>
    <jq-card :show="detailShow" :cardKey="cardKey" :title="title" :tags="tags" @handleClose="handleClose">
      <template slot="new">
        <div style="text-align: left;">
          <div>1、标记 * 为必须输入表单。</div>
        </div>
      </template>
      <template slot="operate" v-if="cardKey">
        <el-button icon="el-icon-edit" type="success" @click="handleUpdate" v-hasPermi="[ 'system:user:edit']" >
          修改
        </el-button>
        <el-button icon="el-icon-delete" type="danger" @click="handleDelete" v-hasPermi="[ 'system:user:remove']" >
          删除
        </el-button>
        <el-button icon="el-icon-key" type="primary" @click="handleResetPwd" v-hasPermi="[ 'system:user:resetPwd']" >
          重置密码
        </el-button>
      </template>
      <template>
        <el-tabs v-model="activeName">
          <el-tab-pane label="用户信息" name="1" lazy>
            <user-update :disabled.sync="disabled" :userId="userId" @handleClose="handleClose"/>
          </el-tab-pane>
          <el-tab-pane label="用户配置" name="2" lazy>
            <user-config :userId="userId" @handleClose="handleClose"/>
          </el-tab-pane>
        </el-tabs>
      </template>
    </jq-card>
  </div>

</template>

<script>
  import { isNullOrEmpty } from '@/utils/jq'
  import { delUser, getUser, resetUserPwd } from '@/api/system/user'
  import UserUpdate from '@/views/system/user/common/userUpdate'
  import UserConfig from '@/views/system/user/common/tab/userConfig'
  export default {
    name: 'userDetail',
    components: { UserConfig, UserUpdate },
    data() {
      return{
        detailShow: false,
        cardKey: null,
        title: '',
        tags: [],
        disabled: true,
        userForm: {},
        activeName: '1',
        userId: null
      }
    },
    props: {
      show: {
        type: Boolean,
        default: false
      },
      value: {
        type: Number
      }
    },
    watch: {
      show(data) {
        this.detailShow = data
      },
      value(data) {
        this.cardKey = data
        if (!isNullOrEmpty(data)) {
          this.disabled = true
        } else {
          this.disabled = false
        }
        this.getUserInfo()
      }
    },
    created() {
      this.getUserInfo()
    },
    methods: {
      getUserInfo() {
        if (isNullOrEmpty(this.cardKey)) {
          //新增
          this.title = '新增用户'
          this.userForm = {}
          this.disabled = false
          this.userId = null
          this.tags = null
        } else {
          getUser(this.cardKey).then(res => {
            this.title = '用户详情'
            this.userForm = res.data
            this.disabled = true
            this.userId = this.userForm.userId
            this.tags = [{ '用户账号': this.userForm.userName }, { '用户昵称': this.userForm.nickName }, { '部门': this.userForm.deptName }, { '手机号码': this.userForm.phonenumber }]
          })
        }
      },
      handleUpdate() {
        this.disabled = false
      },
      /** 关闭操作 */
      handleClose() {
        this.detailShow = false
        this.cardKey = null
        this.$emit('refreshTable')
        this.$emit('handleClose')
      },
      /** 删除按钮操作 */
      handleDelete() {
        const userIds = this.userId
        this.$confirm('是否确认删除用户编号为"' + userIds + '"的数据项?', '警告', {
          confirmButtonText: '确定',
          cancelButtonText: '取消',
          type: 'warning'
        }).then(function() {
          return delUser(userIds)
        }).then(() => {
          this.handleClose()
          this.msgSuccess('删除成功')
        })
      },
      /** 重置密码按钮操作 */
      handleResetPwd() {
        this.$prompt('请输入"' + this.userForm.userName + '"的新密码', '提示', {
          confirmButtonText: '确定',
          cancelButtonText: '取消'
        }).then(({ value }) => {
          resetUserPwd(this.userId, value).then(response => {
            this.handleClose()
            this.msgSuccess('修改成功，新密码是：' + value)
          })
        }).catch(() => {
        })
      }
    }
  }
</script>

<style scoped>

</style>
