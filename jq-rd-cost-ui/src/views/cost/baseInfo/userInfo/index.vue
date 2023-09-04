<template>
  <div class="app-container">
    <el-row :gutter="10" class="table-opt mb8">
      <el-col :span="1.5">
        <el-button type="primary" icon="el-icon-plus" @click="handleAdd"
                   v-hasPermi="['cost:userInfo:add']"
        >
          新增
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button type="info" icon="el-icon-upload2" @click="handleImport"
                   v-hasPermi="['cost:userInfo:import']"
        >
          导入
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button type="warning" icon="el-icon-download" @click="confirmExport"
                   v-hasPermi="['cost:userInfo:export']"
        >
          导出
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button type="danger" icon="el-icon-delete" :disabled="multiple" @click="handleDelete"
                   v-hasPermi="['cost:userInfo:remove']"
        >
          删除
        </el-button>
      </el-col>
    </el-row>

    <jq-table :config="tableConfig" :queryParams.sync="queryParams" ref="JqTableRef" :showSearch.sync="showSearch" @handleSelectionChange="handleSelectionChange">
      <template slot="search">
        <el-form :model="queryParams" ref="queryForm" :inline="true" label-width="100px">
          <el-row>
            <el-col :span="6">
              <el-form-item label="姓名" prop="name">
                <el-input
                  v-model="queryParams.name"
                  placeholder="请输入姓名"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"
                />
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="工号" prop="jobNumber">
                <el-input
                  v-model="queryParams.jobNumber"
                  placeholder="请输入工号"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"
                />
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="人员类别" prop="userType">
                <el-select v-model="queryParams.userType" placeholder="请选择人员类别" class="form-control" clearable filterable>
                  <el-option
                    v-for="dict in userTypeOptions"
                    :key="dict.dictValue"
                    :label="dict.dictLabel"
                    :value="dict.dictValue"
                  ></el-option>
                </el-select>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="所属部门" prop="deptId" class="demonstration">
                <treeselect v-model="queryParams.deptId" placeholder="请输入所属部门" :multiple="false" class="form-control" :options="deptIdOptions"/>
              </el-form-item>
            </el-col>
          </el-row>
          <el-row>
            <el-col :span="6">
              <el-form-item label="费用归集部门" prop="feeDeptId">
                <treeselect v-model="queryParams.feeDeptId" placeholder="请输入费用归集部门" :multiple="false" class="form-control" :options="feeDeptIdOptions"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="职务" prop="duties">
                <el-input
                  v-model="queryParams.duties"
                  placeholder="请输入职务"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"
                />
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="学历" prop="education">
                <el-select v-model="queryParams.education" placeholder="请选择学历" class="form-control" clearable filterable>
                  <el-option
                    v-for="dict in educationOptions"
                    :key="dict.dictValue"
                    :label="dict.dictLabel"
                    :value="dict.dictValue"
                  ></el-option>
                </el-select>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="聘用方式" prop="employMethod">
                <el-select v-model="queryParams.employMethod" placeholder="请选择聘用方式" class="form-control" clearable filterable>
                  <el-option
                    v-for="dict in employMethodOptions"
                    :key="dict.dictValue"
                    :label="dict.dictLabel"
                    :value="dict.dictValue"
                  ></el-option>
                </el-select>
              </el-form-item>
            </el-col>
          </el-row>
          <el-row>
            <el-col :span="6">
              <el-form-item label="职称" prop="title">
                <el-select v-model="queryParams.title" placeholder="请选择职称" class="form-control" clearable filterable>
                  <el-option
                    v-for="dict in titleOptions"
                    :key="dict.dictValue"
                    :label="dict.dictLabel"
                    :value="dict.dictValue"
                  ></el-option>
                </el-select>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="专业" prop="speciality">
                <el-input
                  v-model="queryParams.speciality"
                  placeholder="请输入专业"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"
                />
              </el-form-item>
            </el-col>
          </el-row>
        </el-form>
      </template>
      <template slot="name" slot-scope="{scope}">
        <a @click="userInfoUpdate(scope.row)" v-copy>
          {{ scope.row.name }}
        </a>
      </template>
    </jq-table>

    <!-- 企业人员信息导入组件 -->
    <import-dialog
      :title="importTitle"
      :show.sync="importOpen"
      import-file-name="企业人员信息模板.xlsx"
      @handleQuery="handleQuery"
      :url="importUrl"
    />

    <!-- 企业人员信息导出组件 -->
    <export-dialog
      :show.sync="exportOpen"
      :type.sync="queryParams.exportType"
      @handleExport="handleExport"
    />

    <!-- 企业人员信息查看详情 -->
    <userInfo-detail :show="userInfoCard.show" v-model="userInfoCard.key" @handleClose="userInfoClose"/>

  </div>
</template>

<script>
import JqTableMixin from '@/mixin/JqTable'
import userInfoDetail from '../common/userInfoDetail'
import { getDeptNameInfo } from '@/api/cost/deptInfo'
import { delUserInfo, exportUserInfo } from '@/api/cost/userInfo'

export default {
  name: 'userInfo',
  mixins: [JqTableMixin],
  provide() {
    return {
      handleQuery: this.handleQuery
    }
  },
  data() {
    return {
      // 选中数组
      ids: [],
      // 非单个禁用
      single: true,
      // 非多个禁用
      multiple: true,
      // 显示搜索条件
      showSearch: true,
      // 是否显示导出弹出层
      exportOpen: false,
      //导入弹出层title
      importTitle: '导入',
      // 是否显示导出弹出层
      importOpen: false,
      importUrl: '',
      //人员类别字典
      userTypeOptions: [],
      //部门下拉框
      deptIdOptions: [],
      //费用归集部门下拉框
      feeDeptIdOptions: [],
      //性别字典
      sexOptions: [],
      //学历字典
      educationOptions: [],
      //聘用方式字典
      employMethodOptions: [],
      //职称字典
      titleOptions: [],
      // 查询参数
      queryParams: {},
      //表格配置数据
      tableConfig: {
        url: '/cost/userInfo/list',
        method: 'get',
        queryParams: null,
        orders: 'base_user_info.update_time desc',
        exportUrl: '/cost/userInfo/export',
        globalFlg: true,
        superSearch: {
          keyPlaceholder: '姓名/所属部门/费用归集部门',
          radioSearch: false,
          radioData: [{ label: '标签一', value: 1 }, { label: '标签二', value: 2 }]
        }
      },
      userInfoCard: {
        show: false,
        key: null
      }
    }
  },

  components: {
    userInfoDetail
  },
  created() {
    //人员类别字典
    this.getDicts('base_user_type').then(response => {
      this.userTypeOptions = response.data
    })
    //性别字典
    this.getDicts('sys_user_sex').then(response => {
      this.sexOptions = response.data
    })
    //学历字典
    this.getDicts('education').then(response => {
      this.educationOptions = response.data
    })
    //聘用方式字典
    this.getDicts('engage_mode').then(response => {
      this.employMethodOptions = response.data
    })
    //职称字典
    this.getDicts('position').then(response => {
      this.titleOptions = response.data
    })
    //部门下拉框
    getDeptNameInfo(this.$store.state.item.companyId, this.$store.state.item.itemNo).then(response => {
      this.deptIdOptions = response.data
      this.feeDeptIdOptions = response.data
    })
  },
  methods: {
    /** 搜索按钮操作 */
    handleQuery() {
      this.tableConfig.queryParams = this.queryParams
      this.getJqTableData()
    },

    /** 多选框选中数据 */
    handleSelectionChange(selection) {
      this.ids = selection.map(item => item.id)
      this.single = selection.length !== 1
      this.multiple = !selection.length
    },

    /** 新增按钮操作 */
    handleAdd() {
      this.userInfoCard = {
        show: true,
        key: null
      }
    },

    /** 查看企业人员信息 */
    userInfoUpdate(row) {
      this.userInfoCard = {
        show: true,
        key: row.id
      }
    },

    /** 关闭企业人员信息查看弹框 */
    userInfoClose() {
      this.userInfoCard = {
        show: false,
        row: null
      }
    },

    /** 删除按钮操作 */
    handleDelete(row) {
      const ids = row.id || this.ids
      this.$confirm('是否确认删除企业人员信息编号为"' + ids + '"的数据项?', '警告', {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      }).then(function() {
        return delUserInfo(ids)
      }).then(() => {
        this.getJqTableData()
        this.msgSuccess('删除成功')
        this.handleClose()
        this.handleQuery()
      }).catch(() => {
      })
    },

    /** 打开导入页面按钮操作 */
    handleImport() {
      this.importUrl = '/cost/userInfo/import?companyId=' + this.$store.state.item.companyId + '&itemNo=' + this.$store.state.item.itemNo
      this.importTitle = '企业人员信息导入'
      this.importOpen = true
    },

    /** 打开导出页面按钮 */
    confirmExport() {
      this.queryParams.exportType = 1
      this.exportOpen = true
    },

    /** 确认导出按钮操作 */
    handleExport() {
      if (0 === this.ids.length && 3 === this.queryParams.exportType) {
        this.msgError('未选中任何数据！')
        return
      }
      this.queryParams.ids = this.ids.join(',')
      let queryParams = { ...this.queryParams, ...this.getPageSize() }
      queryParams.menuKey = this.$route.query.menuKey
      exportUserInfo(queryParams)
      this.exportOpen = false
      this.queryParams.ids = []
    }

  }
}
</script>
