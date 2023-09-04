<template>
  <div class="app-container">
    <el-row :gutter="10" class="table-opt mb8">
      <el-col :span="1.5">
        <el-button type="primary" @click="genDevSum">
          生成辅助账汇总表
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button type="primary" @click="genDevFee">
          生成结构明细表
        </el-button>
      </el-col>
    </el-row>
  <el-table :data="summaryTableList"  :summary-method="getSummaries"
            show-summary>
    <el-table-column prop="projectName" label="研发项目名称" width="150"/>
    <el-table-column prop="projectNo" label="项目编号" width="150"/>
    <el-table-column prop="rdForm" label="研发形式" width="150">
      <template slot-scope="scope">
        {{ formatRdForm(scope.row.rdForm) }}
      </template>
    </el-table-column>
    <el-table-column prop="isEntrust" label="委托关联关系" width="150">
      <template slot-scope="scope">
        {{ formatIsEntrust(scope.row.isAssociate, scope.row.isEntrust) }}
      </template>
    </el-table-column>
    <el-table-column prop="isOverseas" label="是否委托境外选项" width="150">
      <template slot-scope="scope">
        {{ formatIsOverseas(scope.row.isAssociate, scope.row.isOverseas) }}
      </template>
    </el-table-column>
    <el-table-column prop="totalMoney11" label="工资薪金" width="150">
    </el-table-column>
    <el-table-column prop="totalMoney12" label="五险一金" width="150">
    </el-table-column>
    <el-table-column prop="totalMoney13" label="外聘研发人员的劳务费用" width="150">
    </el-table-column>
    <el-table-column prop="totalMoney21" label="物料费用" width="150">
    </el-table-column>
    <el-table-column prop="totalMoney22" label="燃料" width="150">
    </el-table-column>
    <el-table-column prop="totalMoney23" label="动力费用" width="150">
    </el-table-column>
    <el-table-column prop="totalMoney31" label="用于研发活动的仪器的折旧费" width="150">
    </el-table-column>
    <el-table-column prop="totalMoney32" label="用于研发活动的设备的折旧费" width="150">
    </el-table-column>
    <el-table-column prop="totalMoney41" label="用于研发活动的软件的摊销费用" width="150">
    </el-table-column>
    <el-table-column prop="totalMoney42" label="用于研发活动的专利权的摊销费用" width="150">
    </el-table-column>
    <el-table-column prop="totalMoney43" label="用于研发活动的非专利技术（包括许可证、专有技术、设计和计算方法等）的摊销费用" width="150">
    </el-table-column>
    <el-table-column prop="totalMoney51" label="新产品设计费" width="150">
    </el-table-column>
    <el-table-column prop="totalMoney52" label="新工艺规程制定费" width="150">
    </el-table-column>
    <el-table-column prop="totalMoney53" label="新药研制的临床试验费" width="150">
    </el-table-column>
    <el-table-column prop="totalMoney54" label="勘探开发技术的现场试验费" width="150">
    </el-table-column>
    <el-table-column prop="totalMoney61" label="其他相关费用" width="150">
    </el-table-column>
<!--    <el-table-column v-for="item in feeItems" :key="item.feeItem" :prop="item.moneyField" :label="item.label" width="180">
      <template slot-scope="scope">
        {{ getMoneyByFeeItem(scope.row, item.feeItem) }}
      </template>
    </el-table-column>-->
  </el-table>
  </div>
</template>

<script>
  import { getSummaryTableList } from '@/api/cost/projectInfo'
  import { isNullOrEmpty } from '@/utils/jq'
  import { Loading } from 'element-ui'
  import axios from 'axios'
  import { getToken } from '@/utils/auth'

  export default {
    name: 'summaryTable',
    data() {
      return {
        //辅助账汇总表数据
        summaryTableList: [],
        queryParams: {
          companyId: this.$store.state.item.companyId,
          itemNo: this.$store.state.item.itemNo
        },
/*        feeItems: [
          { feeItem: '11', label: '工资薪金', moneyField: 'money' },
          { feeItem: '12', label: '五险一金', moneyField: 'money' },
          { feeItem: '13', label: '外聘研发人员的劳务费用', moneyField: 'money' },
          { feeItem: '21', label: '物料费用', moneyField: 'money' },
          { feeItem: '22', label: '燃料', moneyField: 'money' },
          { feeItem: '23', label: '动力费用', moneyField: 'money' },
          { feeItem: '31', label: '用于研发活动的仪器的折旧费', moneyField: 'money' },
          { feeItem: '32', label: '用于研发活动的设备的折旧费', moneyField: 'money' },
          { feeItem: '41', label: '用于研发活动的软件的摊销费用', moneyField: 'money' },
          { feeItem: '42', label: '用于研发活动的专利权的摊销费用', moneyField: 'money' },
          { feeItem: '43', label: '用于研发活动的非专利技术（包括许可证、专有技术、设计和计算方法等）的摊销费用', moneyField: 'money' },
          { feeItem: '51', label: '新产品设计费', moneyField: 'money' },
          { feeItem: '52', label: '新工艺规程制定费', moneyField: 'money' },
          { feeItem: '53', label: '新药研制的临床试验费', moneyField: 'money' },
          { feeItem: '54', label: '勘探开发技术的现场试验费', moneyField: 'money' },
          { feeItem: '61', label: '其他相关费用', moneyField: 'money' }
        ]*/
      }
    },
    watch: {
      '$store.state.item.itemNo'(newVal, oldVal) {
        this.$nextTick(() => {
          this.queryParams.itemNo = this.$store.state.item.itemNo
          this.queryParams.companyId = this.$store.state.item.companyId
          this.getSummaryTable(this.queryParams)
        })
      },
    },
    created() {
      this.getSummaryTable(this.queryParams)
    },
    mounted() {
    },
    methods: {
      /** 获取辅助账汇总表数据 */
      getSummaryTable(query){
        getSummaryTableList(query).then(res=>{
          this.summaryTableList = res.data
        })
      },
      /**研发形式设置 */
      formatRdForm(rdForm) {
        if (rdForm == 1) {
          return '自主研发';
        } else if (rdForm == 2) {
          return '委托研发';
        }else if (rdForm == 3) {
          return '合作研发';
        }
        else if (rdForm == 4) {
          return '集中研发';
        }
      },
      /**是否委托研发*/
      formatIsEntrust(isAssociate, isEntrust) {
        if (isAssociate === 'N') {
          return '不存在';
        } else {
          return isEntrust === 'Y' ? '存在' : '不存在';
        }
      },
      /**是否委托境外*/
      formatIsOverseas(isAssociate, isOverseas) {
        if (isAssociate === 'N') {
          return '非委托项目';
        } else {
          return isOverseas === 'Y' ? '是' : '否';
        }
      },
/*      getMoneyByFeeItem(row, feeItem) {
        if (row.feeItem === feeItem) {
          return row.money;
        } else {
          return null;
        }
      },*/
      /**合计*/
      getSummaries(param) {
        const { columns, data } = param;
        const sums = [];
        columns.forEach((column, index) => {
          if (index === 0) {
            sums[index] = '合计';
            return;
          }
          if (index === 2) {
            sums[index] = '';
            return;
          }
          const values = data.map(item => Number(item[column.property]));
          if (!values.every(value => isNaN(value))) {
            sums[index] = values.reduce((prev, curr) => {
              const value = Number(curr);
              if (!isNaN(value)) {
                return prev + curr;
              } else {
                return prev;
              }
            }, 0);
            sums[index] += '';
          } else {
            sums[index] = '';
          }
        });

        return sums;
      },
      /**生成辅助帐汇总表*/
      genDevSum() {
        this.$confirm('确定生成辅助帐汇总表？', '提示', {
          confirmButtonText: '确定',
          cancelButtonText: '取消',
          type: 'warning'
        }).then(() => {
          let target = document.querySelector('.app-container')
          let loadingInstance = Loading.service({
            target: target, background: '#a6d6ff26', text: '生成中'
          })
          axios({
            method: 'get',
            url: this.$store.getters.apiHostPath + '/gen/itemFile/genDevSum',
            headers: { 'Authorization': 'Bearer ' + getToken() },
            params: this.queryParams
          }).then(res => {
            loadingInstance.close()
            if (res.data.success == 'true') {
              window.open(process.env.VUE_APP_BASE_API + res.data.message)
            } else {
              this.msgError(res.data.message)
            }
          }).catch(e => {
            this.msgError('异常，请联系管理员！')
            loadingInstance.close()
          })
        })
      },
      /**生成结构明细表*/
      genDevFee() {
        this.$confirm('确定生成结构明细表？', '提示', {
          confirmButtonText: '确定',
          cancelButtonText: '取消',
          type: 'warning'
        }).then(() => {
          let target = document.querySelector('.app-container')
          let loadingInstance = Loading.service({
            target: target, background: '#a6d6ff26', text: '生成中'
          })
          axios({
            method: 'get',
            url: this.$store.getters.apiHostPath + '/gen/itemFile/genDevFee',
            headers: { 'Authorization': 'Bearer ' + getToken() },
            params: this.queryParams
          }).then(res => {
            loadingInstance.close()
            if (res.data.success == 'true') {
              window.open(process.env.VUE_APP_BASE_API + res.data.message)
            } else {
              this.msgError(res.data.message)
            }
          }).catch(e => {
            this.msgError('异常，请联系管理员！')
            loadingInstance.close()
          })
        })
      },
    }
  }
</script>

