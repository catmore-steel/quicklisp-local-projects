<template>
  <div class="app-container">
    <el-row :gutter="10" class="table-opt mb8">
      <el-col>
        <div style="margin: 10px 0;">期望无形资产摊销费用：<span style="color:#70B603;">{{ statistics.totalInvisibleFee }}</span></div>
      </el-col>
      <el-col>
        <div style="margin: 0px 0;">前无形资产摊销费用：<span style="color:#D9001B;">{{ statistics.totalDevFee }}</span></div>
      </el-col>
      <el-col>
        <div style="margin: 10px 0;">
          <el-button type="primary" @click="handleClickEnable">拟定无形资产摊销</el-button>
        </div>
      </el-col>
    </el-row>
    <el-row>
      <el-form ref="detail" :model="detail" :rules="detail.rules" label-width="100px" :disabled="disabled">
        <el-table :data="detail.baseIpInfos" style="margin-bottom: 22px;">
          <el-table-column label="资产名称" prop="ipName">
          </el-table-column>
          <el-table-column label="资产类型" prop="ipTypeName">
          </el-table-column>
          <el-table-column label="使用部门" prop="deptName">
          </el-table-column>
          <el-table-column label="无形资产价值" prop="ipMoney">
          </el-table-column>
          <el-table-column label="月摊销额" prop="monthlyAmount">
          </el-table-column>
          <el-table-column label="可用月数" prop="usableMonths">
          </el-table-column>
          <el-table-column label="投入研发摊销月数" prop="budgetIpFee.devMonths">
            <template slot-scope="scope">
              <el-input v-model="scope.row.budgetIpFee.devMonths" @input="val => handleBudgetIpFeeDevMonths(val, scope.row)"></el-input>
            </template>
          </el-table-column>
          <el-table-column label="摊销费用" prop="budgetIpFee.devFee">
            <template slot-scope="scope">
              <el-form-item :prop="'baseIpInfos.' + scope.$index + '.budgetIpFeeDevFee'" :rules="detail.rules.budgetIpFeeDevFee">
                <el-input readonly v-model="scope.row.budgetIpFee.devFee"></el-input>
              </el-form-item>
            </template>
          </el-table-column>
        </el-table>
        <div class="fixed_coperate" style="display: flex;justify-content:center" v-show="!disabled">
          <el-button type="primary" @click="submitForm" :loading="loading">确 定</el-button>
          <el-button @click="handleClickEnable">取 消</el-button>
        </div>
      </el-form>
    </el-row>

  </div>
</template>

<script>
import { Message } from 'element-ui'
import JqTableMixin from '@/mixin/JqTable'
import { isNullOrEmpty } from '@/utils/jq'
import { findBaseIpInfoAll, getStatistics, updateByBaseIpInfo } from '@/api/cost/budgetIpFee.js'

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
      loading:false
    }
  },
  components: {},
  watch: {
    '$store.state.item.itemNo'(newVal, oldVal) {
      this.$nextTick(() => {
        this.itemNo = this.$store.state.item.itemNo
        this.companyId = this.$store.state.item.companyId
        this.budgetIpFeeGetStatistics()
        this.budgetIpFeeFindBaseIpInfoAll()
      })
    }
  },
  created() {
    this.budgetIpFeeGetStatistics()
    this.budgetIpFeeFindBaseIpInfoAll()
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
    budgetIpFeeFindBaseIpInfoAll() {
      findBaseIpInfoAll({
        itemNo: this.itemNo,
        companyId: this.companyId
      }).then(resp => {
        this.detail.baseIpInfos = resp.data
      })
    },

    /** 取消按钮操作 */
    handleClickEnable() {
      let self = this
      self.disabled = !self.disabled
      this.budgetIpFeeFindBaseIpInfoAll();
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
