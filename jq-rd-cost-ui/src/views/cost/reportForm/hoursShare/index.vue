<template>
  <div class="app-container">
    <el-row :gutter="10" class="table-opt mb8">
      <el-col :span="1.5">
        <el-select v-model="queryParams.projectId" placeholder="选择研发项目">
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
        <el-button type="primary" @click="genHoursShare">
          生成工时分摊表
        </el-button>
      </el-col>
    </el-row>
    <el-table :data="shareList">
      <el-table-column prop="month" label="投入月度" width="250"></el-table-column>
      <el-table-column prop="userName" label="参与人员" width="300"></el-table-column>
      <el-table-column prop="devSalaryFeeSum" label="投入研发费用（工资）" width="300"></el-table-column>
      <el-table-column prop="devSocialFeeSum" label="投入研发费用（社保）" width="300"></el-table-column>
      <el-table-column prop="devProvidentFeeSum" label="投入研发费用（公积金）" width="300"></el-table-column>
      <el-table-column label="操作" align="center" class-name="small-padding fixed" fixed="left">
        <template slot-scope="scope">
          <el-button
            type="text"
            icon="el-icon-view"
            @click="shareUpdate(scope.row)"
          >详情
          </el-button>
        </template>
      </el-table-column>
    </el-table>
    <hours-share-detail :show="detailCard.show" v-model="detailCard.key" :month="detailCard.month"
                        @handleClose="shareClose"
    />
  </div>
</template>

<script>
import { getAllProject, listHoursShare } from '@/api/cost/projectInfo'
import HoursShareDetail from '@/views/cost/reportForm/common/hoursShareDetail.vue'
import StageUserDetail from '@/views/cost/reportForm/common/stageUserDetail.vue'
import { Loading } from 'element-ui'
import axios from 'axios'
import { getToken } from '@/utils/auth'

export default {
  name: 'hoursShare',
  components: { StageUserDetail, HoursShareDetail },
  data() {
    return {
      queryParams: {
        companyId: this.$store.state.item.companyId,
        itemNo: this.$store.state.item.itemNo,
        projectId: null,
        month: null
      },
      allProjectList: [],
      shareList: [],
      detailCard: {
        show: false,
        key: null,
        month: null
      }
    }
  },
  watch: {
    '$store.state.item.itemNo'(newVal, oldVal) {
      this.$nextTick(() => {
        this.queryParams.itemNo = this.$store.state.item.itemNo
        this.queryParams.companyId = this.$store.state.item.companyId
        this.projectList(this.queryParams)
      })
    },
    'queryParams.projectId'(val) {
      this.queryParams.projectId = val
      this.listHoursShare(this.queryParams)
    }
  },
  created() {
    this.projectList(this.queryParams)
  },
  methods: {
    projectList(query) {
      getAllProject(query).then(res => {
        this.allProjectList = res.data
        if (this.allProjectList.length > 0) {
          this.queryParams.projectId = this.allProjectList[0].id
        } else {
          this.queryParams.projectId = null
        }
      })
    },
    listHoursShare(query) {
      listHoursShare(query).then(res => {
        this.shareList = res.data
      })
    },
    genHoursShare() {
      this.$confirm('确定生成工时分摊表？', '提示', {
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
          url: this.$store.getters.apiHostPath + '/gen/itemFile/genHoursShare',
          headers: { 'Authorization': 'Bearer ' + getToken() },
          params: this.queryParams
        }).then(res => {
          loadingInstance.close()
          if (res.data.success == 'true') {
            window.open(process.env.VUE_APP_BASE_API + res.data.message)
          } else {
            this.msgError('异常，请联系管理员！')
          }
        }).catch(e => {
          this.msgError('异常，请联系管理员！')
          loadingInstance.close()
        })
      })
    },
    /** 查看研发阶段人员工时分摊明细; */
    shareUpdate(row) {
      this.detailCard = {
        show: true,
        month: row.month,
        key: row.projectId//此处id需替换为业务表唯一索引（非id）
      }
    },
    /** 关闭研发阶段人员工时分摊明细;查看弹框 */
    shareClose() {
      this.detailCard = {
        show: false,
        row: null,
        month: null
      }
    }
  }
}

</script>

<style scoped>

</style>
