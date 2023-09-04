<template>
  <div class="app-container">
    <el-row :gutter="10" class="table-opt mb8">
      <el-col :span="1.5">
        <el-button type="primary" @click="clickDrafting">一键拟定</el-button>
        期望投入人员人工费用 ：<span v-bind:style="{ color: 'green' }">{{statistics.totalUserFee }}</span>
        工时折算人员人工费用 ：<span v-bind:style="{ color: 'red' }">{{ statistics.totalbudgetUserFee }}</span>
      </el-col>
      <!-- <el-col :span="1.5">
         <el-button type="danger" icon="el-icon-delete" :disabled="multiple" @click="handleDelete"
                  v-hasPermi="['cost:budgetUserFee:remove']">
           删除
         </el-button>
       </el-col>-->
    </el-row>

    <jq-table :config="tableConfig" :queryParams.sync="queryParams" ref="JqTableRef" :showSearch.sync="showSearch"
              @handleSelectionChange="handleSelectionChange">
      <template slot="search">
        <el-form :model="queryParams" ref="queryForm" :inline="true" label-width="100px">
          <el-row>
            <el-col :span="6">
              <el-form-item label="员工ID" prop="baseUserId">
                <el-input
                  v-model="queryParams.baseUserId"
                  placeholder="请输入员工ID"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="当月投入研发工时" prop="devHours">
                <el-input
                  v-model="queryParams.devHours"
                  placeholder="请输入当月投入研发工时"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="当月投入人员费用" prop="devSalaryFee">
                <el-input
                  v-model="queryParams.devSalaryFee"
                  placeholder="请输入当月投入人员费用"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
          </el-row>
        </el-form>
      </template>
      <template slot="name" slot-scope="{scope}">
        <a @click="budgetUserFeeUpdate(scope.row)" v-copy>
          {{ scope.row.name }}
        </a>
      </template>
    </jq-table>


    <!---人员费用拟定;导出组件 -->
    <export-dialog :show.sync="exportOpen" :type.sync="queryParams.exportType"
                   @handleExport="handleExport"/>

    <!---人员费用拟定;查看详情 -->
    <budgetUserFee-detail :show="budgetUserFeeCard.show" v-model="budgetUserFeeCard.key"
                          @handleClose="budgetUserFeeClose"/>

  </div>
</template>

<script>
  import JqTableMixin from '@/mixin/JqTable'
  import budgetUserFeeDetail from '../common/budgetUserFeeDetail'
  import { checkDraft, delBudgetUserFee } from '@/api/cost/budgetUserFee'
  import * as budgetUserFeeApi from '@/api/cost/budgetUserFee.js'

  export default {
    name: 'budgetUserFee',
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
        // 是否显示数据
        exportTitle: '导出警告',
        // 是否显示导出弹出层
        exportOpen: false,
        // 查询参数
        queryParams: {},
        //表格配置数据
        tableConfig: {
          url: '/cost/budgetUserFee/list',
          method: 'get',
          queryParams: null,
          exportUrl: '/cost/budgetUserFee/export',
          globalFlg: true,
          superSearch: {
            keyPlaceholder: '',
            radioSearch: true,
            radioData: []
          }
        },
        budgetUserFeeCard: {
          show: false,
          key: null
        },
        statistics: {},
        itemNo: this.$store.state.item.itemNo,
        companyId: this.$store.state.item.companyId
      }
    },
    components: {
      budgetUserFeeDetail
    },
    watch: {
      '$store.state.item.itemNo'(newVal, oldVal) {
        this.$nextTick(() => {
          this.itemNo = this.$store.state.item.itemNo
          this.companyId = this.$store.state.item.companyId
          this.budgetUserFeeGetStatistics()
        })
      }
    },
    created() {
      this.budgetUserFeeGetStatistics()
    },
    methods: {
      budgetUserFeeGetStatistics() {
        let self = this
        budgetUserFeeApi.getStatistics({
          companyId: self.companyId,
          itemNo: self.itemNo
        }).then(resp => {
          self.statistics = resp.data
        })

      },
      clickDrafting() {
        const data = {
          itemNo: this.$store.state.item.itemNo,
          companyId: this.$store.state.item.companyId,
          totalUserFee : this.statistics.totalUserFee,
          totalbudgetUserFee : this.statistics.totalbudgetUserFee
        }
        this.$confirm('是否一键拟定', {
          confirmButtonText: '确定',
          cancelButtonText: '取消',
          type: 'warning'
        }).then(() => {
          return checkDraft(data)
        }).then(() => {
          this.budgetUserFeeGetStatistics()
          this.getJqTableData()
          this.msgSuccess('拟定成功')
        }).catch(() => {
        })
      },

      /** 搜索按钮操作 */
      handleQuery() {
        this.tableConfig.queryParams = this.queryParams
        this.getJqTableData()
        this.budgetUserFeeGetStatistics()
      },

      /** 重置按钮操作 */
      resetQuery() {
        this.resetForm('queryForm')
        this.handleQuery()
        this.budgetUserFeeGetStatistics()
      },

      /** 多选框选中数据 */
      handleSelectionChange(selection) {
        this.ids = selection.map(item => item.id)
        this.single = selection.length !== 1
        this.multiple = !selection.length
      },

      /** 新增按钮操作 */
      handleAdd() {
        this.budgetUserFeeCard = {
          show: true,
          key: null
        }
      },

      /** 查看人员费用拟定; */
      budgetUserFeeUpdate(row) {
        this.budgetUserFeeCard = {
          show: true,
          key: row.id//此处id需替换为业务表唯一索引（非id）
        }
      },

      /** 关闭人员费用拟定;查看弹框 */
      budgetUserFeeClose() {
        this.budgetUserFeeCard = {
          show: false,
          row: null
        }
        this.budgetUserFeeGetStatistics()
      },

      /** 删除按钮操作 */
      handleDelete(row) {
        const ids = row.id || this.ids
        this.$confirm('是否确认删除人员费用拟定;编号为"' + ids + '"的数据项?', '警告', {
          confirmButtonText: '确定',
          cancelButtonText: '取消',
          type: 'warning'
        }).then(function() {
          return delBudgetUserFee(ids)
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
        let queryParams = this.queryParams
        exportBudgetUserFee(queryParams)
        this.exportOpen = false
      }

    }
  }
</script>
