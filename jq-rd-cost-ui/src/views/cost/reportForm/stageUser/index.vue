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
        <el-date-picker
          v-model="queryParams.month"
          type="month"
          value-format="yyyy-MM"
          placeholder="选择月"
        >
        </el-date-picker>
      </el-col>
      <el-col :span="1.5">
        <el-button type="primary" @click="shareStageUser">
          一键分摊
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button type="primary" @click="genStageUser">
          生成工时记录表
        </el-button>
      </el-col>
    </el-row>
    <el-table :data="stageList">
      <el-table-column prop="month" label="投入月度" width="250"></el-table-column>
      <el-table-column prop="userName" label="参与人员" width="250"></el-table-column>
      <el-table-column prop="workingHours" label="总工时" width="250"></el-table-column>
      <el-table-column prop="devHours" label="当月投入该项目研发工时" width="400"></el-table-column>
      <el-table-column prop="workRate" label="研发工时比例" width="300"></el-table-column>
      <el-table-column label="操作" align="center" class-name="small-padding fixed" fixed="left">
        <template slot-scope="scope">
          <el-button
            type="text"
            icon="el-icon-view"
            @click="detailUpdate(scope.row)"
          >详情
          </el-button>
        </template>
      </el-table-column>
    </el-table>

    <stage-user-detail :show="detailCard.show" v-model="detailCard.key"
                       @handleClose="detailClose"
    />
  </div>
</template>

<script>
import { getAllProject, listStageUser, shareStageUser } from '@/api/cost/projectInfo'
import StageUserDetail from '@/views/cost/reportForm/common/stageUserDetail.vue'
import { Loading } from 'element-ui'
import axios from 'axios'
import { getToken } from '@/utils/auth'
import { isNullOrEmpty } from '@/utils/jq'

export default {
  name: 'stageUser',
  components: { StageUserDetail },
  data() {
    return {
      queryParams: {
        companyId: this.$store.state.item.companyId,
        itemNo: this.$store.state.item.itemNo,
        projectId: null,
        month: null
      },
      allProjectList: [],
      stageList: [],
      detailCard: {
        show: false,
        key: null
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
    'queryParams.month'(val) {
      this.queryParams.month = val
      this.listStageUser(this.queryParams)
    },
    'queryParams.projectId'(val) {
      this.queryParams.projectId = val
      this.listStageUser(this.queryParams)
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
        // this.queryParams.month = new Date().getFullYear() + '-01'
      })
    },
    listStageUser(query) {
      listStageUser(query).then(res => {
        this.stageList = res.data
      })
    },
    shareStageUser() {
      if (isNullOrEmpty(this.queryParams.month)) {
        this.msgError('点击分摊时，月份不能为空！')
      } else {
        shareStageUser(this.queryParams).then(res => {
          if (res.code === 200) {
            this.msgSuccess('分摊成功')
          }
        })
      }
    },
    genStageUser() {
      this.$confirm('确定生成工时记录表？', '提示', {
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
          url: this.$store.getters.apiHostPath + '/gen/itemFile/genStageUser',
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
    /** 查看研发阶段人员工时分摊明细; */
    detailUpdate(row) {
      this.detailCard = {
        show: true,
        key: row.id//此处id需替换为业务表唯一索引（非id）
      }
    },
    /** 关闭研发阶段人员工时分摊明细;查看弹框 */
    detailClose() {
      this.detailCard = {
        show: false,
        row: null
      }
    }
  }
}
</script>

<style scoped>

</style>
