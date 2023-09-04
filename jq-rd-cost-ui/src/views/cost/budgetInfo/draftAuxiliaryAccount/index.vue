<template>
  <div class="app-container">
    <el-row :gutter="10" class="table-opt mb8">
      <el-col>
        <div style="margin: 10px 0;">期望无形资产摊销费用：<span style="color:#70B603;">{{ statistics.totalInvisibleFee }}</span></div>
      </el-col>
    </el-row>
    <el-row>
        <el-table :data="draftAuxiliaryAccounts">
          <el-table-column label="时间月度" prop="ipName" width="200">
          </el-table-column>
          <el-table-column label="工资薪酬" prop="ipTypeName" width="200">
          </el-table-column>
          <el-table-column label="五险一金" prop="deptName" width="200">
          </el-table-column>
          <el-table-column label="外聘研发人员的劳务费用" prop="ipMoney" width="200">
          </el-table-column>
          <el-table-column label="燃料" prop="monthlyAmount" width="200">
          </el-table-column>
          <el-table-column label="动力费用" prop="usableMonths" width="200">
          </el-table-column>
          <el-table-column label="用于中间试验和产品试制的模具、工艺装备开发及制造费" prop="budgetIpFee.devMonths" width="200">
          </el-table-column>
          <el-table-column label="用于不构成固定资产的样品、样机及一般测试手段购置费" prop="budgetIpFee.devFee" width="200">
          </el-table-column>
          <el-table-column label="用于试制产品的检验费" prop="budgetIpFee.devFee" width="200">
          </el-table-column>
          <el-table-column label="用于研发活动的仪器、设备的运行维护、调整、检验、维修等费用" prop="budgetIpFee.devFee" width="200">
          </el-table-column>
          <el-table-column label="通过经营租赁方式租入的用于研发活动的仪器、设备租赁费" prop="budgetIpFee.devFee" width="200">
          </el-table-column>
          <el-table-column label="用于研发活动的仪器的折旧费" prop="budgetIpFee.devFee" width="200">
          </el-table-column>
          <el-table-column label="用于研发活动的设备的折旧费" prop="budgetIpFee.devFee" width="200">
          </el-table-column>
          <el-table-column label="用于研发活动的软件的摊销费用" prop="budgetIpFee.devFee" width="200">
          </el-table-column>
          <el-table-column label="用于研发活动的专利权的摊销费用" prop="budgetIpFee.devFee" width="200">
          </el-table-column>
          <el-table-column label="用于研发活动的非专利技术（包括许可证、专有技术、设计和计算方法等）的摊销费用" prop="budgetIpFee.devFee" width="200">
          </el-table-column>
          <el-table-column label="新产品设计费用" prop="budgetIpFee.devFee" width="200">
          </el-table-column>
          <el-table-column label="其他相关费用" prop="budgetIpFee.devFee" width="200">
          </el-table-column>
        </el-table>
    </el-row>

  </div>
</template>

<script>
import { Message } from 'element-ui'
import JqTableMixin from '@/mixin/JqTable'
import { isNullOrEmpty } from '@/utils/jq'
import * as apiDraftAuxiliaryAccount from '@/api/cost/draftAuxiliaryAccount.js'

export default {
  name: 'budgetIpFee',
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
      disabled: true,
      // 查询参数
      queryParams: {},
      //表格配置数据
      feeCard: {
        show: false,
        key: null
      },
      itemNo: this.$store.state.item.itemNo,
      companyId: this.$store.state.item.companyId,
      detail: {
        baseIpInfos: [],
        rules: {
          budgetIpFeeDevFee: [{
            required: true, validator: (rule, value, callback) => {
              let sumDevFee = this.detail.baseIpInfos
                .map(elt => Number(elt.budgetIpFee.devFee))
                .reduce((prev, curr) => prev + curr, 0)
              if (sumDevFee > this.statistics.totalInvisibleFee) {
                Message.error('当前摊销费用总计:' + sumDevFee + ',不能大于期望无形资产摊销费用:' + this.statistics.totalInvisibleFee)
                return callback(new Error('err'))
              }
              return callback()
            }, trigger: 'change'
          }]
        }
      },
      statistics: {},
      loading:false,
      draftAuxiliaryAccounts:[]
    }
  },
  components: {},
  watch: {
    '$store.state.item.itemNo'(newVal, oldVal) {
      this.$nextTick(() => {
        this.itemNo = this.$store.state.item.itemNo
        this.companyId = this.$store.state.item.companyId
        this.budgetIpFeeGetStatistics()
        this.listDraftAuxiliaryAccount()
      })
    }
  },
  created() {
    this.budgetIpFeeGetStatistics()
    this.listDraftAuxiliaryAccount()
  },
  methods: {
    /** 计算摊销费用 */
    handleBudgetIpFeeDevMonths(val, row) {
      row.budgetIpFee.devMonths = val.replace(/^([0-9]\d*\.?\d{0,2})?.*$/, '$1')
      if (row.budgetIpFee.devMonths) {
        row.budgetIpFee.devFee = row.monthlyAmount * row.budgetIpFee.devMonths
      }
    },

    /** 无形资产摊销费用统计 */
    budgetIpFeeGetStatistics() {
      if (!isNullOrEmpty(this.itemNo) && !isNullOrEmpty(this.companyId)) {
        getStatistics({ itemNo: this.itemNo, companyId: this.companyId }).then(resp => {
          this.statistics = resp.data
        })
      }
    },

    /** 无形资产摊销明细 */
    listDraftAuxiliaryAccount() {
      apiDraftAuxiliaryAccount.list({
        itemNo: this.itemNo,
        companyId: this.companyId
      }).then(resp => {
        this.draftAuxiliaryAccounts = resp.data
      })
    },

    /** 取消按钮操作 */
    handleClickEnable() {
      let self = this
      self.disabled = !self.disabled
      this.listDraftAuxiliaryAccount();
    },
    submitForm() {
      let self = this;
      this.$refs['detail'].validate(valid => {
        if (valid) {
          if (true) {
            let budgetIpFees = []
            this.detail.baseIpInfos.forEach(baseIpInfo => {
              baseIpInfo.budgetIpFee.ipId = baseIpInfo.id
              baseIpInfo.budgetIpFee.companyId = baseIpInfo.companyId
              baseIpInfo.budgetIpFee.itemNo = baseIpInfo.itemNo
              baseIpInfo.budgetIpFee.delFlag = '0'
              budgetIpFees.push(baseIpInfo.budgetIpFee)
            })
            self.loading = true;
            updateByBaseIpInfo({ budgetIpFees: budgetIpFees }).then(response => {
              self.loading = false;
              if (response.code === 200) {
                this.msgSuccess(response.msg)
                this.budgetIpFeeGetStatistics()
                this.handleClickEnable()
              }
            })
          }
        }
      })
    }

  }
}
</script>
