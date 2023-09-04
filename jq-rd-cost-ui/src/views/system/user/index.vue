<template>
  <div class="app-container">
    <el-row :gutter="20">
      <!--部门数据-->
      <el-col :span="4" :xs="24" style="border-right: 2px solid #bbd1ad;    height: calc(100vh - 127px);overflow: auto;">
        <div class="head-container">
          <el-input
            v-model="deptName"
            placeholder="请输入部门名称"
            clearable
            prefix-icon="el-icon-search"
            style="margin-bottom: 20px"
          />
        </div>
        <div class="head-container">
          <el-tree
            :data="deptOptions"
            :props="defaultProps"
            :expand-on-click-node="false"
            :filter-node-method="filterNode"
            :default-expanded-keys="[1]"
            node-key="id"
            ref="tree"
            @node-click="handleNodeClick"
          />
        </div>
      </el-col>
      <!--用户数据-->
      <el-col :span="20" :xs="24">
        <el-row :gutter="10" class="table-opt mb8">
          <el-col :span="1.5">
            <el-button
              type="primary"
              icon="el-icon-plus"
              @click="handleAdd"
              v-hasPermi="['system:user:add']"
            >新增
            </el-button>
          </el-col>
          <!--<el-col :span="1.5">
            <el-button
              type="danger"
              icon="el-icon-upload2"
              @click="handleImport"
              v-hasPermi="['system:user:import']"
            >导入
            </el-button>
          </el-col>-->
        </el-row>

        <jq-table :config="tableConfig" ref="JqTableRef" :queryParams.sync="queryParams" :showSearch.sync="showSearch" @handleSelectionChange="handleSelectionChange">
          <template slot="search">
            <el-form :model="queryParams" ref="queryForm" :inline="true" label-width="68px">
              <el-row>
                <el-col :span="6">
                  <el-form-item label="用户账号" prop="userName">
                    <el-input
                      v-model="queryParams.userName"
                      placeholder="请输入用户账号"
                      clearable
                      style="width: 240px"
                    />
                  </el-form-item>
                </el-col>
                <el-col :span="6">
                  <el-form-item label="手机号码" prop="phonenumber">
                    <el-input
                      v-model="queryParams.phonenumber"
                      placeholder="请输入手机号码"
                      clearable
                      style="width: 240px"
                    />
                  </el-form-item>
                </el-col>
                <el-col :span="6">
                  <el-form-item label="状态" prop="status">
                    <el-select
                      v-model="queryParams.status"
                      placeholder="用户状态"
                      clearable
                      style="width: 240px"
                    >
                      <el-option
                        v-for="dict in statusOptions"
                        :key="dict.dictValue"
                        :label="dict.dictLabel"
                        :value="dict.dictValue"
                      />
                    </el-select>
                  </el-form-item>
                </el-col>
                <el-col :span="6">
                  <el-form-item label="创建时间">
                    <el-date-picker
                      v-model="queryParams.createDateRange"
                      style="width: 240px"
                      value-format="yyyy-MM-dd"
                      type="daterange"
                      range-separator="-"
                      start-placeholder="开始日期"
                      end-placeholder="结束日期"
                    ></el-date-picker>
                  </el-form-item>
                </el-col>
              </el-row>
            </el-form>
          </template>
          <template slot="userName" slot-scope="{scope}">
            <a @click="userInfoUpdate(scope.row)" style="color: #1890ff">
              {{ scope.row.userName }}
            </a>
          </template>
          <template slot="status" slot-scope="{scope}">
            <el-switch
              v-model="scope.row.status"
              active-value="0"
              inactive-value="1"
              @change="handleStatusChange(scope.row)"
            ></el-switch>
          </template>
        </jq-table>
      </el-col>
    </el-row>

    <user-detail :show="userCard.show" v-model="userCard.key" @handleClose="userInfoClose"/>

    <import-dialog :title="importTitle"
                   :show.sync="importOpen"
                   import-file-name="用户信息导入模板.xlsx"
                   @handleQuery="handleQuery"
                   url="/system/user/import"
    />
  </div>
</template>

<script>
import { addUser, changeUserStatus, delUser, exportUser, getUser, importTemplate, listUser, resetUserPwd, updateUser } from '@/api/system/user'
import { getToken } from '@/utils/auth'
import { treeselect } from '@/api/system/dept'
import Treeselect from '@riophae/vue-treeselect'
import '@riophae/vue-treeselect/dist/vue-treeselect.css'
import JqTableMixin from '@/mixin/JqTable'
import UserDetail from '@/views/system/user/common/userDetail'

export default {
  name: 'User',
  mixins: [JqTableMixin],
  components: { UserDetail, Treeselect },
  data() {
    return {
      // 遮罩层
      loading: true,
      // 选中数组
      ids: [],
      // 非单个禁用
      single: true,
      // 非多个禁用
      multiple: true,
      // 显示搜索条件
      showSearch: true,
      // 总条数
      total: 0,
      // 用户表格数据
      userList: null,
      // 弹出层标题
      title: '',
      // 部门树选项
      deptOptions: undefined,
      // 是否显示弹出层
      open: false,
      // 部门名称
      deptName: undefined,
      // 默认密码
      initPassword: undefined,
      // 日期范围
      dateRange: [],
      // 状态数据字典
      statusOptions: [],
      // 性别状态字典
      sexOptions: [],
      // 岗位选项
      postOptions: [],
      // 角色选项
      roleOptions: [],
      // 是否状态字典
      yesNoOptions: [],
      //导入弹出层title
      importTitle: '导入',
      // 是否显示导出弹出层
      importOpen: false,
      // 表单参数
      form: {},
      tableConfig: {
        url: '/system/user/list',
        method: 'get',
        queryParams: null,
        superSearch: {
          keyPlaceholder: '登录账号/用户昵称/用户手机号/用户部门',
          radioSearch: false
        }
      },
      userCard: {
        show: false,
        key: null
      },
      defaultProps: {
        children: 'children',
        label: 'label'
      },
      // 用户导入参数
      upload: {
        // 是否显示弹出层（用户导入）
        open: false,
        // 弹出层标题（用户导入）
        title: '',
        // 是否禁用上传
        isUploading: false,
        // 是否更新已经存在的用户数据
        updateSupport: 0,
        // 设置上传的请求头部
        headers: { Authorization: 'Bearer ' + getToken() },
        // 上传的地址
        url: process.env.VUE_APP_BASE_API + '/system/user/importData'
      },
      // 查询参数
      queryParams: {
        createDateRange: [],
      },
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
  watch: {
    // 根据名称筛选部门树
    deptName(val) {
      this.$refs.tree.filter(val)
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
  },
  methods: {
    /** 查询用户列表 */
    getList() {
      this.loading = true
      listUser(this.addDateRange(this.queryParams, this.dateRange)).then(response => {
          this.userList = response.rows
          this.total = response.total
          this.loading = false
        }
      )
    },
    /** 查询部门下拉树结构 */
    getTreeselect() {
      treeselect().then(response => {
        this.deptOptions = response.data
      })
    },
    // 筛选节点
    filterNode(value, data) {
      if (!value) return true
      return data.label.indexOf(value) !== -1
    },
    // 节点单击事件
    handleNodeClick(data) {
      this.queryParams.deptId = data.id
      this.tableConfig.queryParams = this.queryParams
      this.getJqTableData()
    },
    // 用户状态修改
    handleStatusChange(row) {
      let text = row.status === '0' ? '启用' : '停用'
      this.$confirm('确认要"' + text + '""' + row.userName + '"用户吗?', '警告', {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      }).then(function() {
        return changeUserStatus(row.userId, row.status)
      }).then(() => {
        this.msgSuccess(text + '成功')
      }).catch(function() {
        row.status = row.status === '0' ? '1' : '0'
      })
    },
    // 取消按钮
    cancel() {
      this.open = false
      this.reset()
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
    /** 搜索按钮操作 */
    handleQuery() {
      this.queryParams.page = 1
      this.tableConfig.queryParams = this.queryParams
      this.getJqTableData()
    },
    /** 重置按钮操作 */
    resetQuery() {
      this.createDateRange = []
      this.resetForm('queryForm')
      this.handleQuery()
    },
    // 多选框选中数据
    handleSelectionChange(selection) {
      this.ids = selection.map(item => item.userId)
      this.single = selection.length != 1
      this.multiple = !selection.length
    },
    /** 新增按钮操作 */
    handleAdd() {
      this.userCard = {
        show: true,
        key: null
      }
    },
    /** 修改按钮操作 */
    handleUpdate(row) {
      this.reset()
      this.getTreeselect()
      const userId = row.userId || this.ids
      getUser(userId).then(response => {
        this.form = response.data
        this.postOptions = response.posts
        this.roleOptions = response.roles
        this.form.postIds = response.postIds
        this.form.roleIds = response.roleIds
        this.open = true
        this.title = '修改用户'
        this.form.password = ''
      })
    },
    /** 重置密码按钮操作 */
    handleResetPwd(row) {
      this.$prompt('请输入"' + row.userName + '"的新密码', '提示', {
        confirmButtonText: '确定',
        cancelButtonText: '取消'
      }).then(({ value }) => {
        resetUserPwd(row.userId, value).then(response => {
          this.msgSuccess('修改成功，新密码是：' + value)
        })
      }).catch(() => {
      })
    },
    /** 提交按钮 */
    submitForm: function() {
      this.$refs['form'].validate(valid => {
        if (valid) {
          if (this.form.userId != undefined) {
            updateUser(this.form).then(response => {
              this.msgSuccess('修改成功')
              this.open = false
              this.getJqTableData()
            })
          } else {
            addUser(this.form).then(response => {
              this.msgSuccess('新增成功')
              this.open = false
              this.getJqTableData()
            })
          }
        }
      })
    },
    /** 删除按钮操作 */
    handleDelete(row) {
      const userIds = row.userId || this.ids
      this.$confirm('是否确认删除用户编号为"' + userIds + '"的数据项?', '警告', {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      }).then(function() {
        return delUser(userIds)
      }).then(() => {
        this.getList()
        this.msgSuccess('删除成功')
      })
    },
    /** 导出按钮操作 */
    handleExport() {
      const queryParams = this.queryParams
      this.$confirm('是否确认导出所有用户数据项?', '警告', {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      }).then(function() {
        return exportUser(queryParams)
      }).then(response => {
        this.download(response.msg)
      })
    },
    /** 导入按钮操作 */
    handleImport() {
      this.importTitle = '用户信息导入'
      this.importOpen = true
    },
    /** 下载模板操作 */
    importTemplate() {
      importTemplate().then(response => {
        this.download(response.msg)
      })
    },
    // 文件上传中处理
    handleFileUploadProgress(event, file, fileList) {
      this.upload.isUploading = true
    },
    // 文件上传成功处理
    handleFileSuccess(response, file, fileList) {
      this.upload.open = false
      this.upload.isUploading = false
      this.$refs.upload.clearFiles()
      this.$alert(response.msg, '导入结果', { dangerouslyUseHTMLString: true })
      this.getJqTableData()
    },
    // 提交上传文件
    submitFileForm() {
      this.$refs.upload.submit()
    },
    userInfoUpdate(row) {
      this.userCard = {
        show: true,
        key: row.userId
      }
    },
    userInfoClose() {
      this.userCard = {
        show: false,
        row: null
      },
        this.handleQuery()
    }
  }
}
</script>
