<template>
  <div>
    <el-form ref="form" :model="form" :rules="rules" :disabled="disabled" label-width="80px">
      <el-row>
        <el-col :span="12">
          <el-form-item v-if="form.userId == undefined" label="登录账号" prop="userName">
            <el-input v-model="form.userName" placeholder="请输入登录账号"/>
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item v-if="form.userId == undefined" label="登录密码" prop="password">
            <el-input v-model="form.password" placeholder="请输入登录密码" type="text"/>
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="12">
          <el-form-item label="用户昵称" prop="nickName">
            <el-input v-model="form.nickName" placeholder="请输入用户昵称"/>
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="归属部门" prop="deptId">
            <treeselect v-model="form.deptId" :options="deptOptions" :show-count="true" placeholder="请选择归属部门"  class="form-control"/>
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="12">
          <el-form-item label="手机号码" prop="phonenumber">
            <el-input v-model="form.phonenumber" placeholder="请输入手机号码" maxlength="15"/>
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="邮箱" prop="email">
            <el-input v-model="form.email" placeholder="请输入邮箱" maxlength="50"/>
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="12">
          <el-form-item label="用户性别">
            <el-select v-model="form.sex" placeholder="请选择">
              <el-option
                v-for="dict in sexOptions"
                :key="dict.dictValue"
                :label="dict.dictLabel"
                :value="dict.dictValue"
              ></el-option>
            </el-select>
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="身份证号" prop="cardNo">
            <el-input v-model="form.cardNo" placeholder="请输入身份证号"/>
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="12">
          <el-form-item label="岗位">
            <el-select v-model="form.postIds" filterable clearable multiple placeholder="请选择">
              <el-option
                v-for="item in postOptions"
                :key="item.postId"
                :label="item.postName"
                :value="item.postId"
                :disabled="item.status === '1'"
              ></el-option>
            </el-select>
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="角色">
            <el-select v-model="form.roleIds" filterable clearable multiple placeholder="请选择">
              <el-option
                v-for="item in roleOptions"
                :key="item.roleId"
                :label="item.roleName"
                :value="item.roleId"
                :disabled="item.status === '1'"
              ></el-option>
            </el-select>
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="12">
          <el-form-item label="是否主管">
            <el-radio-group v-model="form.isLeader">
              <el-radio
                v-for="dict in yesNoOptions"
                :key="dict.dictValue"
                :label="dict.dictValue"
              >{{ dict.dictLabel }}
              </el-radio>
            </el-radio-group>
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="状态">
            <el-radio-group v-model="form.status">
              <el-radio
                v-for="dict in statusOptions"
                :key="dict.dictValue"
                :label="dict.dictValue"
              >{{ dict.dictLabel }}
              </el-radio>
            </el-radio-group>
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="12">
          <el-form-item label="代理师">
            <el-radio-group v-model="form.isCertificate">
              <el-radio
                v-for="dict in yesNoOptions"
                :key="dict.dictValue"
                :label="dict.dictValue"
              >{{ dict.dictLabel }}
              </el-radio>
            </el-radio-group>
          </el-form-item>
        </el-col>
      </el-row>
      <el-row v-if="form.isCertificate === 'Y'">
        <el-col :span="12">
          <el-form-item label="执业证号" prop="certificateNo" :rules="form.isCertificate === 'Y' ? rules.certificateNo : [{ required: false }]">
            <el-input v-model="form.certificateNo" placeholder="请输入执业证号"/>
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="座机号码" prop="landLine">
            <el-input v-model="form.landLine" placeholder="请输入公司座机号码"/>
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="24">
          <el-form-item label="备注">
            <el-input v-model="form.remark" type="textarea" placeholder="请输入内容"></el-input>
          </el-form-item>
        </el-col>
      </el-row>
    </el-form>
    <div class="fixed_coperate" v-show="!disabled">
      <el-button type="primary" @click="submitForm">确 定</el-button>
      <el-button @click="handleClose">取 消</el-button>
    </div>
  </div>
</template>

<script>
  import { treeselect } from '@/api/system/dept'
  import { isNullOrEmpty } from '@/utils/jq'
  import { addUser, getUser, updateUser } from '@/api/system/user'

  export default {
    name: 'userUpdate',
    data() {
      return {
        // 表单参数
        form: {},
        // 部门树选项
        deptOptions: [],
        // 部门名称
        deptName: undefined,
        // 默认密码
        initPassword: undefined,
        // 状态数据字典
        statusOptions: [],
        // 是否状态字典
        yesNoOptions: [],
        // 性别状态字典
        sexOptions: [],
        // 岗位选项
        postOptions: [],
        // 角色选项
        roleOptions: [],
        // 表单校验
        rules: {
          userName: [
            { required: true, message: '用户账号不能为空', trigger: 'blur' },
            {
              pattern: /^[_a-zA-Z0-9]+$/,
              message: '用户账号应由英文大小写字母、数字、下划线组成',
              trigger: 'blur'
            }
          ],
          nickName: [
            { required: true, message: '用户昵称不能为空', trigger: 'blur' }
          ],
          deptId: [
            { required: true, message: '归属部门不能为空', trigger: 'blur' }
          ],
          password: [
            { required: true, message: '用户密码不能为空', trigger: 'blur' }
          ],
          phonenumber: [
            { required: true, message: '手机号码不能为空', trigger: 'blur' },
            {
              pattern: /^1[3|4|5|6|7|8|9][0-9]\d{8}$/,
              message: '请输入正确的手机号码',
              trigger: 'blur'
            }
          ],
          cardNo: [
            {
              pattern: /(^[1-9]\d{5}(18|19|([23]\d))\d{2}((0[1-9])|(10|11|12))(([0-2][1-9])|10|20|30|31)\d{3}[0-9Xx]$)|(^[1-9]\d{5}\d{2}((0[1-9])|(10|11|12))(([0-2][1-9])|10|20|30|31)\d{2}$)/,
              message: '请输入正确的身份证号码',
              trigger: 'blur'
            }
          ],
          certificateNo: [
            { required: true, message: '执业证号不能为空', trigger: 'blur' }
          ]
        }
      }
    },
    props: {
      userId: {
        type: Number,
        require: true
      },
      disabled: {
        type: Boolean,
        require: true
      }
    },
    watch: {
      userId(val) {
        if (isNullOrEmpty(val)) {
          this.reset()
        } else {
          this.getUserInfo(val)
        }
      }
    },
    created() {
      this.getTreeselect()
      this.getDicts('sys_normal_disable').then(response => {
        this.statusOptions = response.data
      })
      this.getDicts('sys_user_sex').then(response => {
        this.sexOptions = response.data
      })
      this.getConfigKey('sys.user.initPassword').then(response => {
        this.initPassword = response.msg
      })
      this.getDicts('sys_yes_no').then(response => {
        this.yesNoOptions = response.data
      })
      this.getUserInfo(this.userId)
    },
    methods: {
      getUserInfo(userId) {
        if (isNullOrEmpty(userId)) {
          this.reset()
          this.getTreeselect()
          getUser().then(response => {
            this.postOptions = response.posts
            this.roleOptions = response.roles
            this.form.password = this.initPassword
          })
        } else {
          getUser(userId).then(response => {
            this.form = response.data
            this.postOptions = response.posts
            this.roleOptions = response.roles
            this.form.postIds = response.postIds
            this.form.roleIds = response.roleIds
            this.form.password = ''
          })
        }
      },
      // 表单重置
      reset() {
        this.form = {
          userId: undefined,
          deptId: undefined,
          userName: undefined,
          nickName: undefined,
          password: undefined,
          phonenumber: undefined,
          email: undefined,
          sex: undefined,
          status: '0',
          isCertificate: 'N',
          remark: undefined,
          cardNo: undefined,
          postIds: [],
          roleIds: [],
          isLeader: 'N'
        }
        this.resetForm('form')
      },
      /** 查询部门下拉树结构 */
      getTreeselect() {
        treeselect().then(response => {
          this.deptOptions = response.data
        })
      },
      handleClose() {
        this.$emit('handleClose')
      },
      /** 提交按钮 */
      submitForm: function() {
        this.$refs['form'].validate(valid => {
          if (valid) {
            if (this.form.userId != undefined) {
              updateUser(this.form).then(response => {
                this.msgSuccess('修改成功')
                this.handleClose()
              })
            } else {
              addUser(this.form).then(response => {
                this.msgSuccess(response.msg)
                this.handleClose()
              })
            }
          }
        })
      },
    }
  }
</script>

<style scoped>

</style>
