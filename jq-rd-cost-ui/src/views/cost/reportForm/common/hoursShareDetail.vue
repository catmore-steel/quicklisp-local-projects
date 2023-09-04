<template>
  <jq-card :show="cardShow" :cardKey="cardKey" :title="title" :tags="tags" @handleClose="handleClose">
    <template slot="new">
      <div style="text-align: left;">
        <div>1、标记 * 为必须输入表单。</div>
      </div>
    </template>
    <template slot="operate" v-if="cardKey">
      <!--      <el-button icon="el-icon-edit" type="success" @click="handleUpdate"
            >
              修改
            </el-button>-->
    </template>
    <template>
      <el-tabs v-model="activeName">
        <el-tab-pane label="记录详情" name="1">
          <el-table :data="stageUserForm.shareList" :summary-method="getSummaries" show-summary>
            <el-table-column prop="userName" label="姓名" width="120"></el-table-column>
            <el-table-column prop="salaryFee" label="应付工资" width="125"></el-table-column>
            <el-table-column prop="safeFee" label="应付社保" width="125"></el-table-column>
            <el-table-column prop="goldFee" label="应付公积金" width="125"></el-table-column>
            <el-table-column prop="workingHours" label="总工时" width="125"></el-table-column>
            <el-table-column prop="devHours" label="投入研发工时" width="125"></el-table-column>
            <el-table-column prop="devSalaryFee" label="投入研发费用（工资）" width="125"></el-table-column>
            <el-table-column prop="devSocialFee" label="投入研发费用（社保）" width="125"></el-table-column>
            <el-table-column prop="devProvidentFee" label="投入研发费用（公积金）" width="125"></el-table-column>
          </el-table>
        </el-tab-pane>
      </el-tabs>
    </template>
  </jq-card>
</template>

<script>
import { isNullOrEmpty } from '@/utils/jq'
import { getHoursShare } from '@/api/cost/projectInfo'

export default {
  name: 'hoursShareDetail',
  components: {},
  provide() {
    return {
      handleComplete: this.handleComplete
    }
  },
  data() {
    return {
      title: '',
      stageUserForm: {},
      activeName: '1',
      cardShow: false,
      cardKey: null,
      tags: [],
      stageUserId: null,
      edit: false,  //开启编辑
      disabled: false
    }
  },
  props: {
    show: {
      type: Boolean,
      default: false
    },
    value: {
      type: Number
    },
    month: {
      String
    }
  },
  watch: {
    show(data) {
      this.cardShow = data
    },
    month(data) {
      this.activeName = '1'
      this.cardKey = data
      if (data) {
        this.disabled = true
      } else {
        this.disabled = false
      }
      this.getDetail()
    }
  },
  created() {
    this.getDetail()
  },
  mounted() {
  },
  methods: {
    /** 修改操作 */
    handleUpdate() {
      this.disabled = false
    },

    /** 关闭 */
    handleClose() {
      this.cardShow = false
      this.cardKey = null
      this.$emit('handleClose')
    },

    // 获取研发阶段人员投入明细详情
    getDetail() {
      let projectId = this.value
      let month = this.month
      if (isNullOrEmpty(projectId) || isNullOrEmpty(month)) {
        //新增
        this.title = '新增研发阶段人员投入明细'
        this.stageUserId = null
        this.tags = null
      } else {
        getHoursShare({ projectId: projectId, month: month }).then(res => {
          let data = res.data
          this.stageUserForm = res.data
          this.title = '分摊详情'
          this.stageUserId = data.id
          this.tags = [{ '投入月度': data.month }, { '投入人员费用（工资）': data.devSalaryFeeSum },
            { '投入人员费用（社保）': data.devSocialFeeSum }, { '投入人员费用（公积金）': data.devProvidentFeeSum }]
        })
      }
    },
    getSummaries(param) {
      const sums = []
      const { columns, data } = param
      let salaryFeeSum = 0
      let safeFeeSum = 0
      let goldFeeSum = 0
      let workingHoursSum = 0
      let devHoursSum = 0
      let devSalaryFeeSum = 0
      let devSocialFeeSum = 0
      let devProvidentFeeSum = 0
      if (!isNullOrEmpty(this.stageUserForm.shareList)) {
        this.stageUserForm.shareList.forEach(e => {
          salaryFeeSum += e.salaryFee
          safeFeeSum += e.safeFee
          goldFeeSum += e.goldFee
          workingHoursSum += e.workingHours
          devHoursSum += e.devHours
          devSalaryFeeSum += e.devSalaryFee
          devSocialFeeSum += e.devSocialFee
          devProvidentFeeSum += e.devProvidentFee
        })
      }
      columns.forEach((column, index) => {
        if (index === 0) {
          sums[index] = '合计'
          return
        }
        if (index === 1) {
          sums[index] = salaryFeeSum.toFixed(2)
          return
        }
        if (index === 2) {
          sums[index] = safeFeeSum.toFixed(2)
          return
        }
        if (index === 3) {
          sums[index] = goldFeeSum.toFixed(2)
          return
        }
        if (index === 4) {
          sums[index] = workingHoursSum.toFixed(2)
          return
        }
        if (index === 5) {
          sums[index] = devHoursSum.toFixed(2)
          return
        }
        if (index === 6) {
          sums[index] = devSalaryFeeSum.toFixed(2)
          return
        }
        if (index === 7) {
          sums[index] = devSocialFeeSum.toFixed(2)
          return
        }
        if (index === 8) {
          sums[index] = devProvidentFeeSum.toFixed(2)
          return
        }
      })
      return sums
    }
  }
}
</script>
