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
          <el-table :data="stageUserForm.detailList" :summary-method="getSummaries" show-summary>
            <el-table-column prop="devDate" label="时间" width="250"></el-table-column>
            <el-table-column prop="workingHour" label="标准工时" width="250"></el-table-column>
            <el-table-column prop="otherHours" label="投入其他研发项目工时" width="300"></el-table-column>
            <el-table-column prop="devHour" label="投入该项目研发工时"></el-table-column>
          </el-table>
        </el-tab-pane>
      </el-tabs>
    </template>
  </jq-card>
</template>

<script>
import { isNullOrEmpty } from '@/utils/jq'
import { getStageUser } from '@/api/cost/projectInfo'

export default {
  name: 'stageUserDetail',
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
    }
  },
  watch: {
    show(data) {
      this.cardShow = data
    },
    value(data) {
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
      let stageUserId = this.value
      if (isNullOrEmpty(stageUserId)) {
        //新增
        this.title = '新增研发阶段人员投入明细'
        this.stageUserId = null
        this.tags = null
      } else {
        getStageUser(stageUserId).then(res => {
          let data = res.data
          this.stageUserForm = res.data
          this.title = '记录详情'
          this.stageUserId = data.id
          this.tags = [{ '参与人员': data.userName }, { '投入月度': data.month }, { '当月总工时': data.workingHours }, { '当月投入项目研发工时': data.devHours }]
        })
      }
    },
    getSummaries(param) {
      const sums = []
      const { columns, data } = param
      let workingHourSum = 0
      let otherHoursSum = 0
      let devHourSum = 0
      if (!isNullOrEmpty(this.stageUserForm.detailList)) {
        this.stageUserForm.detailList.forEach(e => {
          workingHourSum += e.workingHour
          otherHoursSum += e.otherHours
          devHourSum += e.devHour
        })
      }
      columns.forEach((column, index) => {
        if (index === 0) {
          sums[index] = '合计'
          return
        }
        if (index === 1) {
          sums[index] = workingHourSum.toFixed(2)
          // sums[index] = this.stageUserForm.workingHours
          return
        }
        if (index === 2) {
          sums[index] = otherHoursSum.toFixed(2)
          return
        }
        if (index === 3) {
          sums[index] = devHourSum.toFixed(2)
          return
        }
      })
      return sums
    }
  }
}
</script>
