<template>
  <div class="app-container">
    <el-row :gutter="10" class="table-opt mb8">
      <el-col :span="1.5">
        <el-button type="info" icon="el-icon-upload2" @click="handleImport"
                   v-hasPermi="['cost:userInfo:import']"
        >
          导入
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button type="danger" icon="el-icon-delete" :disabled="multiple" @click="handleDelete"
                   v-hasPermi="['cost:userSalary:remove']"
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
              <el-form-item label="员工姓名" prop="userName">
                <el-input
                  v-model="queryParams.userName"
                  placeholder="请输入员工姓名"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"
                />
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="所属部门" prop="deptName">
                <el-input
                  v-model="queryParams.deptName"
                  placeholder="请输入所属部门"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"
                />
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="薪酬月份" prop="salaryMonth">
                <el-date-picker
                  v-model="queryParams.salaryMonth"
                  style="width: 100%"
                  value-format="yyyy-MM"
                  type="month"
                  placeholder="选择月份"
                  clearable
                ></el-date-picker>
              </el-form-item>
            </el-col>
          </el-row>
        </el-form>
      </template>
    </jq-table>

    <!-- 企业人员工资记录导入组件 -->
    <import-dialog
      :title="importTitle"
      :show.sync="importOpen"
      import-file-name="人员工资记录导入模板.xlsx"
      @handleQuery="handleQuery"
      :url="importUrl"
    />

    <!-- 企业人员工资记录;查看详情 -->
    <userSalary-detail :show="userSalaryCard.show" v-model="userSalaryCard.key" @handleClose="userSalaryClose"/>

  </div>
</template>

<script>
import JqTableMixin from '@/mixin/JqTable'
import userSalaryDetail from '../common/userSalaryDetail'
import { delUserSalary, exportUserSalary } from '@/api/cost/userSalary'

export default {
  name: 'userSalary',
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
      // 导入弹出层title
      importTitle: '导入',
      // 是否显示导入弹出层
      importOpen: false,
      // 导入资源路径
      importUrl: '',
      // 查询参数
      queryParams: {},
      //表格配置数据
      tableConfig: {
        url: '/cost/userSalary/list',
        method: 'get',
        queryParams: null,
        orders: 'base_user_salary_detail.update_time desc',
        exportUrl: '/cost/userSalary/export',
        globalFlg: true,
        superSearch: {
          keyPlaceholder: '员工姓名/所属部门/薪酬月份',
          radioSearch: true,
          radioData: [{ label: '研发人员', value: 1 }, { label: '非研发人员', value: 2 }]
        }
      },
      userSalaryCard: {
        show: false,
        key: null
      },
      itemNo: this.$store.state.item.itemNo,
      companyId: this.$store.state.item.companyId,
    }
  },
  components: {
    userSalaryDetail
  },
  watch: {
    '$store.state.item.itemNo'(newVal, oldVal) {
      this.$nextTick(() => {
        this.itemNo = this.$store.state.item.itemNo
        this.companyId = this.$store.state.item.companyId
      })
    }
  },
  created() {
  },
  methods: {
    /** 搜索按钮操作 */
    handleQuery() {
      this.tableConfig.queryParams = this.queryParams
      this.getJqTableData()
    },

    /** 重置按钮操作 */
    resetQuery() {
      this.resetForm('queryForm')
      this.handleQuery()
    },

    /** 多选框选中数据 */
    handleSelectionChange(selection) {
      this.ids = selection.map(item => item.id)
      this.single = selection.length !== 1
      this.multiple = !selection.length
    },

    /** 查看企业人员工资记录; */
    userSalaryUpdate(row) {
      this.userSalaryCard = {
        show: true,
        key: row.id
      }
    },

    /** 关闭企业人员工资记录;查看弹框 */
    userSalaryClose() {
      this.userSalaryCard = {
        show: false,
        row: null
      }
    },

    /** 打开导入页面按钮操作 */
    handleImport() {
      this.importTitle = '人员工资记录导入'
      this.importOpen = true
      this.importUrl = '/cost/userSalary/import?companyId=' + this.$store.state.item.companyId + '&itemNo=' + this.$store.state.item.itemNo
    },

    /** 删除按钮操作 */
    handleDelete(row) {
      const ids = row.id || this.ids
      this.$confirm('是否确认删除企业人员工资记录;编号为"' + ids + '"的数据项?', '警告', {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      }).then(function() {
        return delUserSalary(ids)
      }).then(() => {
        this.getJqTableData()
        this.msgSuccess('删除成功')
      }).catch(() => {
      })
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
      exportUserSalary(queryParams)
      this.exportOpen = false
    }

  }
}
</script>
