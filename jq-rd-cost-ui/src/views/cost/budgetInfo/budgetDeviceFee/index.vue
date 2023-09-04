<template>
  <div class="app-container">
    <el-row :gutter="10" class="table-opt mb8">
      <el-col :span="1.5">
        <div>期望折旧费用:<span style="color:#70B603;">{{ statistics.totalEquipmentFee }}</span></div>
      </el-col>
      <el-col :span="1.5">
        <div>摊销折旧费用：<span style="color:#D9001B;">{{ statistics.totalDevFee }}</span></div>
      </el-col>
    </el-row>

    <jq-table :config="tableConfig" :queryParams.sync="queryParams" ref="JqTableRef" :showSearch.sync="showSearch" @handleSelectionChange="handleSelectionChange">
      <template slot="search">
        <el-form :model="queryParams" ref="queryForm" :inline="true" label-width="100px">
          <el-row>
            <el-col :span="6">
              <el-form-item label="设备名称" prop="deviceName">
                <el-input
                  v-model="queryParams.deviceName"
                  placeholder="请输入设备名称"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"
                />
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="设备编码" prop="deviceNo">
                <el-input
                  v-model="queryParams.deviceNo"
                  placeholder="请输入设备编码"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"
                />
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="设备类型" prop="deviceType">
                <el-select v-model="queryParams.deviceType" placeholder="请选择设备类型" class="form-control" clearable filterable>
                  <el-option
                    v-for="dict in deviceTypeOptions"
                    :key="dict.dictValue"
                    :label="dict.dictLabel"
                    :value="dict.dictValue"
                  ></el-option>
                </el-select>
              </el-form-item>
            </el-col>
          </el-row>
        </el-form>
      </template>
      <template slot="deviceNo" slot-scope="{scope}">
        <a @click="feeUpdate(scope.row)" v-copy>
          {{ scope.row.deviceNo }}
        </a>
      </template>
    </jq-table>


    <!-- 研发投入设备折旧费用拟定导出组件 -->
    <export-dialog :show.sync="exportOpen" :type.sync="queryParams.exportType" @handleExport="handleExport"/>

    <!-- 研发投入设备折旧费用拟定查看详情 -->
    <fee-detail :show="feeCard.show" v-model="feeCard.key" @handleClose="feeClose"/>

  </div>
</template>

<script>
import JqTableMixin from '@/mixin/JqTable'
import feeDetail from './common/feeDetail'
import { getStatistics } from '@/api/cost/budgetDeviceFee.js'
import { isNullOrEmpty } from '@/utils/jq'

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
        url: '/cost/budgetDeviceFee/deviceInfo/list',
        method: 'get',
        queryParams: null,
        orders: 'base_device_info.update_time desc',
        exportUrl: '/system/fee/export',
        globalFlg: true,
        superSearch: {
          keyPlaceholder: '',
          radioSearch: true,
          radioData: []
        }
      },
      feeCard: {
        show: false,
        key: null
      },
      itemNo: this.$store.state.item.itemNo,
      companyId: this.$store.state.item.companyId,
      statistics: {},
      deviceTypeOptions: []
    }
  },
  components: {
    feeDetail
  },
  watch: {
    '$store.state.item.itemNo'(newVal, oldVal) {
      this.$nextTick(() => {
        this.itemNo = this.$store.state.item.itemNo
        this.companyId = this.$store.state.item.companyId
        this.budgetDeviceFeeGetStatistics()
      })
    }
  },
  created() {
    this.budgetDeviceFeeGetStatistics()
    this.getDicts('device_type').then(response => {
      this.deviceTypeOptions = response.data
    })
  },
  methods: {
    /** 搜索按钮操作 */
    handleQuery() {
      this.tableConfig.queryParams = this.queryParams
      this.getJqTableData()
      this.budgetDeviceFeeGetStatistics()
    },

    /** 多选框选中数据 */
    handleSelectionChange(selection) {
      this.ids = selection.map(item => item.id)
      this.single = selection.length !== 1
      this.multiple = !selection.length
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
      this.budgetDeviceFeeGetStatistics()
    },

    /** 折旧费用统计 */
    budgetDeviceFeeGetStatistics() {
      if (!isNullOrEmpty(this.itemNo) && !isNullOrEmpty(this.companyId)) {
        getStatistics({ itemNo: this.itemNo, companyId: this.companyId }).then(resp => {
          this.statistics = resp.data
        })
      }
    }
  }
}
</script>
