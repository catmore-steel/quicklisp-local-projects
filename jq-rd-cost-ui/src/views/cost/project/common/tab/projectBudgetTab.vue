<template>
  <div class="slash-table">
    <jq-panel title="项目预算">
      <div style="margin-bottom: 20px;">
        <span>
          项目预算总计：{{ budgetFeeTotal }} 元
        </span>
      </div>
      <el-table :data="projectBudgetList" border show-summary>
        <el-table-column prop="feeType" label="年度" width="250" align="center">
          <el-table-column prop="feeType" label="费用类型" align="center" width="250"></el-table-column>
        </el-table-column>
        <el-table-column prop="fee" :label="yearLabel" align="center">
          <template slot-scope="scope">
            <el-input v-model="scope.row.fee" @input="sumBudgetFee(scope.row)"/>
          </template>
        </el-table-column>
        <el-table-column prop="feeTotal" label="合计（元）" align="center"></el-table-column>
      </el-table>
    </jq-panel>
    <div class="fixed_coperate" v-show="!disabled">
      <el-button type="primary" @click="submitProjectBudget">确 定</el-button>
      <el-button @click="cancel">取 消</el-button>
    </div>
  </div>
</template>
<script>
import { getProjectBudgetList, submitProjectBudget } from '@/api/cost/projectInfo'
import { isNullOrEmpty } from '@/utils/jq'

export default {
  name: 'projectBudgetTab',
  components: {},
  props: {
    projectInfoId: {
      type: Number
    },
    disabled: {
      type: Boolean,
      require: true
    }
  },
  watch: {
    projectInfoId(val) {
      this.activeName = '1'
      this.getProjectBudget(val)
    }
  },
  data() {
    return {
      //研发项目预算费用数据
      projectBudgetList: [],
      //预算费用总计
      budgetFeeTotal: null,
      //年度为研发项目起止日期跨越年度
      yearLabel: ''
    }
  },
  created() {
    this.getProjectBudget(this.projectInfoId)
  },
  methods: {

    /** 获取研发项目预算数据 */
    getProjectBudget(projectInfoId) {
      getProjectBudgetList(projectInfoId).then(res => {
        this.projectBudgetList = res.data.costProjectBudgetList
        this.budgetFeeTotal = res.data.budgetFeeTotal
        this.yearLabel = res.data.startDate.split('-')[0]
      })
    },

    /** 费用计算 */
    sumBudgetFee(row){
      row.feeTotal = row.fee
      this.budgetFeeTotal = 0
      this.projectBudgetList.forEach(item=>{
        if(!isNullOrEmpty(item.fee)){
          this.budgetFeeTotal += Number(item.fee)
        }
      })
    },

    /** 项目预算提交 */
    submitProjectBudget(){
      const data = {
        id: this.projectInfoId,
        costProjectBudgetList: this.projectBudgetList
      }
      submitProjectBudget(data).then(res=>{
        this.msgSuccess(res.msg)
      })
    },

    /** 取消按钮 */
    cancel() {
      if (this.projectInfoId) {
        this.$emit('update:disabled', true)
      }
    },
  }
}
</script>
<style scoped lang="scss">

.slash-table {
  ::v-deep .el-table thead.is-group th {
    background: none;
    padding: 0px;
  }

  ::v-deep .el-table thead.is-group tr:first-of-type th:first-of-type {
    border-bottom: none; /*中间的横线去掉*/
  }

  ::v-deep .el-table thead.is-group tr:first-of-type th:first-of-type div.cell {
    text-align: right; /*上边文字靠右*/
  }

  ::v-deep .el-table thead.is-group tr:last-of-type th:first-of-type div.cell {
    text-align: left; /*下边文字靠左*/
  }

  /*核心代码*/

  ::v-deep .el-table thead.is-group tr:first-of-type th:first-of-type:before {
    content: "";
    position: absolute;
    width: 1px;
    height: 200px; /*斜线的长度*/
    top: 0;
    left: 0;
    background-color: grey;
    opacity: 0.2;
    display: block;
    transform: rotate(-77.8deg); /*调整斜线的角度*/
    -webkit-transform-origin: top;
    transform-origin: top;
  }

  ::v-deep .el-table thead.is-group tr:last-of-type th:first-of-type:before {
    content: "";
    position: absolute;
    width: 1px;
    height: 200px; /*斜线的长度*/
    bottom: 0;
    right: 0;
    background-color: grey;
    opacity: 0.2;
    display: block;
    transform: rotate(-77.8deg); /*调整斜线的角度*/
    -webkit-transform-origin: bottom;
    transform-origin: bottom;
  }

  ::v-deep .el-table thead.is-group th {
    height: 27.4px;
  }

}
</style>
