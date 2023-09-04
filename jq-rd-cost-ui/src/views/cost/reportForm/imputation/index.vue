<template>
 <div class="app-container">
    <el-row :gutter="10" class="table-opt mb8">
      <el-col :span="1.5">
        <el-select v-model="queryParams.costProjectInfoId" placeholder="请选择研发项目" @change="handleQuery">
          <el-option
            v-for="item in allProjectList"
            :key="item.id"
            :label="`${item.projectNo} - ${item.projectName}`"
            :value="item.id"
          >
          </el-option>
        </el-select>
      </el-col>
      <el-col :span="1.5">
        <el-select v-model="queryParams.feeItem" placeholder="请选择费用项目" @change="handleQuery">
          <el-option
            v-for="dict in feeItemOptions"
            :key="dict.dictValue"
            :label="dict.dictLabel"
            :value="dict.dictValue"
          ></el-option>
        </el-select>
      </el-col>
      <el-col :span="1.5">
        <el-button type="primary" @click="handleAssociate">
          关联原始序时账
        </el-button>
      </el-col>
    </el-row>
    <jq-table :config="tableConfig" :queryParams.sync="queryParams" ref="JqTableRef" @handleSelectionChange="handleSelectionChange" :isExport="false"
    >
    </jq-table>
    <export-dialog :show.sync="exportOpen" :type.sync="queryParams.exportType"
                   @handleExport="handleExport"
    />
    <jq-dialog title="关联原始序时账" :visible.sync="projectOpen" fullscreen append-to-body>
      <accountInfo :row="row" :projectOpen.sync="projectOpen" @handleQuery="handleQuery"></accountInfo>
    </jq-dialog>
  </div>
</template>

<script>
import JqTableMixin from '@/mixin/JqTable'

import { getStatistics } from '@/api/cost/budgetDeviceFee.js'
import accountInfo from './accountInfo'

import { getAllProject } from '@/api/cost/projectInfo'
import { exportAccountInfo } from '@/api/cost/accountInfo'


export default {
  name: 'budgetDeviceFee',
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
      auxiliaryList: [],
      //研发项目下拉框
      allProjectList: [],
      feeItemOptions: [],
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
        url: '/cost/feeCollectInfo/listCollectionOfRDExpenses',
        method: 'get',
        queryParams: null,
        orders: 'base_device_info.update_time desc',
        exportUrl: '/system/fee/export',
        globalFlg: true
      },
      feeCard: {
        show: false,
        key: null
      },
      itemNo: this.$store.state.item.itemNo,
      companyId: this.$store.state.item.companyId,
      statistics: {},
      deviceTypeOptions: [],
      projectOpen:false,
      row:{}
    }
  },
  components: {
    accountInfo
  },
  watch: {
    '$store.state.item.itemNo'(newVal, oldVal) {
      this.$nextTick(() => {
        this.itemNo = this.$store.state.item.itemNo
        this.companyId = this.$store.state.item.companyId
        this.projectList()
      })
    }
  },
  created() {
    this.getDicts('fee_item').then(response => {
      this.feeItemOptions = response.data
      //this.queryParams.feeItem = this.feeItemOptions[0].dictValue
    })
    this.projectList()
  },
  methods: {
    projectList() {
      getAllProject({itemNo:this.itemNo,companyId:this.companyId}).then(res => {
        this.allProjectList = res.data
      })
    },
    /** 搜索按钮操作 */
    handleQuery() {
      this.tableConfig.queryParams = this.queryParams
      this.getJqTableData()
    },

    /** 多选框选中数据 */
    handleSelectionChange(selection) {
      console.log('selection',selection)
      this.ids = selection.map(item => item.id)
      this.single = selection.length !== 1
      this.multiple = !selection.length
      this.row=selection[0]||{};
    },

    /** 查看研发投入设备折旧费用拟定 */
    feeUpdate(row) {
      this.feeCard = {
        show: true,
        key: row.id//此处id需替换为业务表唯一索引（非id）
      }
    },

    /** 关闭研发投入设备折旧费用拟定查看弹框 */
    feeClose() {
      this.feeCard = {
        show: false,
        row: null
      }
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
      exportAccountInfo(queryParams)
      this.exportOpen = false
    },
    handleAssociate() {
      if (this.single) {
        this.$message({
          showClose: true,
          message: '请选择一条记录!',
          type: 'warning'
        })
        return;
      } else {
        this.row.feeItem=this.queryParams.feeItem
        this.row.projectId=this.queryParams.costProjectInfoId
        this.projectOpen=true;

      }
    }
  }
}
</script>

<style lang="scss" scoped>

::v-deep .el-icon-unlock {
      display: none;
}

</style>
