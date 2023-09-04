<template>
  <div class="budgetMaterialFee">
    <el-row :gutter="20">
      <el-col :span="6" :xs="24">
        <div class="col1">
          <div style="margin: 10px 0;">
            <el-button type="primary" @click="handleClickEnable">拟定直接投入</el-button>
          </div>
          <div style="margin: 10px 0;">期望研发直接投入费用：<span style="color:#70B603;">{{ statistics.totalMaterialFee }}</span></div>
          <div style="margin: 10px 0;">当前研发直接投入费用：<span style="color:#D9001B;">{{ statistics.totalbudgetMaterialFee }}</span></div>
          <div style="margin: 20px 0;">
            <el-table :data="baseAccountInfoData" style="margin-bottom: 22px;">
              <el-table-column label="入账月度" :sortable="false" prop="accDateYm">
              </el-table-column>
              <el-table-column prop="sumCreditAmount" label="研发直接投入(初拟上限)">
              </el-table-column>
            </el-table>
          </div>
        </div>
      </el-col>
      <el-col :span="18" :xs="24">
        <div class="col2">
          <jq-panel title="直接投入明细">
            <el-form :model="productRuleForm" ref="productRuleForm" :rules="productRuleForm.rules" :disabled="disabled">
              <el-table :data="productRuleForm.tableData" border show-summary>
                <el-table-column label="月度" :sortable="false" prop="month">
                </el-table-column>
                <el-table-column label="材料总费用" :sortable="false" prop="materialFee">
                  <template slot-scope="scope">
                  <el-form-item :prop="'tableData.' + scope.$index + '.materialFee'">
                      <el-input v-model="scope.row.materialFee"
                                @input="val => scope.row.materialFee = val.replace(/^([0-9]\d*\.?\d{0,2})?.*$/,'$1')"
                      ></el-input>
                  </el-form-item>
                  </template>
                </el-table-column>
                <el-table-column label="燃料总费用" :sortable="false" prop="fuelFee">
                  <template slot-scope="scope">
                  <el-form-item :prop="'tableData.' + scope.$index + '.fuelFee'">
                      <el-input v-model="scope.row.fuelFee"
                                @input="val => scope.row.fuelFee = val.replace(/^([0-9]\d*\.?\d{0,2})?.*$/,'$1')"
                      ></el-input>
                  </el-form-item>
                  </template>
                </el-table-column>
                <el-table-column label="动力总费用" :sortable="false" prop="powerFee">
                  <template slot-scope="scope">
                  <el-form-item :prop="'tableData.' + scope.$index + '.powerFee'">
                      <el-input v-model="scope.row.powerFee"
                                @input="val => scope.row.powerFee = val.replace(/^([0-9]\d*\.?\d{0,2})?.*$/,'$1')"
                      ></el-input>
                  </el-form-item>
                </template>
                </el-table-column>
                <el-table-column prop="devMaterialFee" label="投入研发材料费用">
                  <template slot-scope="scope">
                    <el-form-item :prop="'tableData.' + scope.$index + '.devMaterialFee'" :rules="productRuleForm.rules.devMaterialFee">
                      <el-input v-model="scope.row.devMaterialFee"
                                @input="val => scope.row.devMaterialFee = val.replace(/^([0-9]\d*\.?\d{0,2})?.*$/,'$1')"
                      ></el-input>
                    </el-form-item>
                  </template>
                </el-table-column>
                <el-table-column prop="devFuelFee" label="投入研发燃料费用">
                  <template slot-scope="scope">
                    <el-form-item :prop="'tableData.' + scope.$index + '.devFuelFee'" :rules="productRuleForm.rules.devFuelFee">
                      <el-input readonly placeholder="自动计算" v-model="scope.row.devFuelFee" @input="val => scope.row.devFuelFee = val.replace(/^([0-9]\d*\.?\d{0,2})?.*$/,'$1')"></el-input>
                    </el-form-item>
                  </template>
                </el-table-column>
                <el-table-column prop="devPowerFee" label="投入研发动力费用">
                  <template slot-scope="scope">
                    <el-form-item :prop="'tableData.' + scope.$index + '.devPowerFee'" :rules="productRuleForm.rules.devPowerFee">
                      <el-input readonly placeholder="自动计算" v-model="scope.row.devPowerFee" @input="val => scope.row.devPowerFee = val.replace(/^([0-9]\d*\.?\d{0,2})?.*$/,'$1')" ></el-input>
                    </el-form-item>
                  </template>
                </el-table-column>
              </el-table>
              <div style="text-align: center;margin: 20px 0;">
                <el-button type="primary" @click="submitForm" :loading="loading">确 定</el-button>
                <el-button @click="handleClickEnable2">取 消</el-button>
              </div>
            </el-form>
          </jq-panel>
        </div>
      </el-col>
    </el-row>
  </div>
</template>

<script>
import { Message } from 'element-ui'
import JqTableMixin from '@/mixin/JqTable'
import { isNullOrEmpty } from '@/utils/jq'
import * as budgetMaterialFeeApi from '@/api/cost/budgetMaterialFee.js'

export default {
  name: 'budgetMaterialFee',
  mixins: [JqTableMixin],
  provide() {
    return {
      handleQuery: this.handleQuery
    }
  },
  data() {
    let checkSumCreditAmount = (rule, value, callback) => {
      let idx = rule.field.split('\.')[1]
      let idxData = this.productRuleForm.tableData[idx]
      let idxGroupDataSum = this.productRuleForm.tableData
        .filter(elt => elt.groupAccDateYm == idxData.groupAccDateYm)
        .map(elt => Number(elt.devMaterialFee) + Number(elt.devFuelFee) + Number(elt.devPowerFee))
        .reduce((prev, curr) => prev + curr, 0)
      if (idxGroupDataSum > idxData.sumCreditAmount) {
        Message.error('当前入账月度:' + idxData.groupAccDateYm + '不能大于入账月度的研发直接投入累计')
        return callback(new Error('当前入账月度:' + idxData.groupAccDateYm + '不能大于入账月度的研发直接投入累计'))
      }
      return callback()
    }

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
        url: '/cost/budgetMaterialFee/list',
        method: 'get',
        queryParams: null,
        orders: 'budget_material_fee.update_time desc',
        exportUrl: '/system/fee/export',
        superSearch: {
          keyPlaceholder: '',
          radioSearch: true,
          radioData: [{ label: '搜索一', value: 1 }, { label: '搜索二', value: 2 }]
        }
      },
      feeCard: {
        show: false,
        key: null
      },
      baseAccountInfoData: [],
      //budgetMaterialFees:[],
      itemNo: this.$store.state.item.itemNo,
      companyId: this.$store.state.item.companyId,
      loading: false,
      statistics: {},
      disabled: true,
      productRuleForm: {
        tableData: [],
        rules: {
          devMaterialFee: [{ required: true, validator: checkSumCreditAmount, trigger: 'blur' }],
          devFuelFee: [{ required: true, validator: checkSumCreditAmount, trigger: 'blur' }],
          devPowerFee: [{ required: true, validator: checkSumCreditAmount, trigger: 'blur' }]
        }
      }
    }
  },
  components: {},
  watch: {
    'productRuleForm.tableData': {
      handler(newVal, oldVal) {
        console.log('handler***',newVal, oldVal)
        if (newVal) {
          newVal.forEach(elt=>{
            if (elt.materialFee > 0 && elt.devMaterialFee>=0) {
              elt.devFuelFee=elt.fuelFee*elt.devMaterialFee/elt.materialFee
              elt.devPowerFee=elt.powerFee*elt.devMaterialFee/elt.materialFee
            }
          })
        }
      },
      deep: true
    },
    '$store.state.item.itemNo'(newVal, oldVal) {
      this.$nextTick(() => {
        this.itemNo = this.$store.state.item.itemNo
        this.companyId = this.$store.state.item.companyId
        this.budgetMaterialFeeGetStatistics()
        this.budgetMaterialFeeFindGroupingByAccDateYm()
      })
    }
  },
  created() {
    this.budgetMaterialFeeGetStatistics()
    this.budgetMaterialFeeFindGroupingByAccDateYm()
  },
  methods: {
    /** 搜索按钮操作 */
    handleQuery() {
      this.tableConfig.queryParams = this.queryParams
      this.getJqTableData()
    },

    /** 研发直接投入费用 */
    budgetMaterialFeeGetStatistics() {
      if (!isNullOrEmpty(this.companyId) && !isNullOrEmpty(this.itemNo)) {
        budgetMaterialFeeApi.getStatistics({
          companyId: this.companyId,
          itemNo: this.itemNo
        }).then(resp => {
          this.statistics = resp.data
        })
      }
    },

    /** 入账月度明细 */
    budgetMaterialFeeFindGroupingByAccDateYm() {
      let self = this
      budgetMaterialFeeApi.findGroupingByAccDateYm({
        companyId: self.companyId,
        itemNo: self.itemNo,
        accountType: '2',
        orders: 'base_account_info.acc_date asc'
      }).then(resp => {
        self.baseAccountInfoData = resp.data
        self.budgetMaterialFeeListAll()
      })
    },

    /** 直接投入明细 */
    budgetMaterialFeeListAll() {
      let self = this
      if (self.companyId != null && self.itemNo != null) {
        budgetMaterialFeeApi.listAll({
          companyId: self.companyId,
          itemNo: self.itemNo
        }).then(resp => {
          self.productRuleForm.tableData = resp.data
          self.baseAccountInfoData.forEach((baseAccountInfo, i) => {
            if (i == 0) {
              baseAccountInfo.budgetMaterialFees = self.productRuleForm.tableData
                .filter(elt => {
                  return baseAccountInfo.accDateYm >= elt.month
                })
            } else {
              baseAccountInfo.budgetMaterialFees = self.productRuleForm.tableData
                .filter(elt => {
                  return baseAccountInfo.accDateYm >= elt.month && self.baseAccountInfoData[i - 1].accDateYm < elt.month
                })
            }
            //分组
            baseAccountInfo.budgetMaterialFees.forEach(elt => {
              elt.groupAccDateYm = baseAccountInfo.accDateYm
              elt.sumCreditAmount = baseAccountInfo.sumCreditAmount
            })
          })
        })
      }
    },

    /** 确定按钮操作 */
    submitForm() {
      let self = this
      self.$refs['productRuleForm'].validate((valid) => {
        if (valid) {
          self.loading=true;
          budgetMaterialFeeApi.saveOrUpdateByBudgetMaterialFeeForm({ budgetMaterialFees: this.productRuleForm.tableData }).then(response => {
            self.loading=false;
            if (response.code === 200) {
              this.msgSuccess(response.msg);
              this.handleClickEnable();
            }
          })
        }
      })
    },

    /** 取消按钮操作 */
    handleClickEnable() {
      let self = this
      self.disabled = !self.disabled
    },
    handleClickEnable2(){
      this.disabled = !this.disabled
      this.budgetMaterialFeeFindGroupingByAccDateYm()
    }
  }
}
</script>
<style lang="scss" scoped>
.budgetMaterialFee {
  background-color: rgba(245, 247, 250, 1);
  padding: 10px;

  .col1 {
    height: 740px;
    background: inherit;
    background-color: inherit;
    background-color: rgba(255, 255, 255, 1);
    box-sizing: border-box;
    border-width: 1px;
    border-style: solid;
    border-color: rgba(233, 233, 233, 1);
    border-radius: 0px;
    -moz-box-shadow: none;
    -webkit-box-shadow: none;
    box-shadow: none;
    font-family: '微软雅黑', sans-serif;
    font-weight: 400;
    font-style: normal;
    text-align: left;
    line-height: 20px;
    text-align: center;
    padding: 5px;
  }

  .col2 {
    height: 740px;
    background: inherit;
    background-color: inherit;
    background-color: rgba(255, 255, 255, 1);
    box-sizing: border-box;
    border-width: 1px;
    border-style: solid;
    border-color: rgba(233, 233, 233, 1);
    border-radius: 0px;
    -moz-box-shadow: none;
    -webkit-box-shadow: none;
    box-shadow: none;
    font-family: '微软雅黑', sans-serif;
    font-weight: 400;
    font-style: normal;
    text-align: left;
    line-height: 20px;
  }
}
</style>
