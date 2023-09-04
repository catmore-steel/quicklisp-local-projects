<template>
  <div class="app-container">
    <el-row>
      <el-col :span="11">
        <span>研发项目摊销情况  2023</span>
        <el-button type="primary" icon="el-icon-plus" @click="setProjectInitProportion" style="float: right">
          配置研发项目初始化占比
        </el-button>
        <div ref="projectAmortizationChart" style="width: 100%; height: 300px;margin-top: 20px"></div>
      </el-col>
      <el-col :span="2" style="margin-top: 60px; text-align: center; height: 280px;">
        <el-divider direction="vertical"></el-divider>
      </el-col>
      <el-col :span="11">
        <span>年度研发费用投入情况  2023</span>
        <div ref="projectPutIntoChart" style="width: 100%; height: 300px;margin-top: 20px"></div>
      </el-col>
    </el-row>
    <el-row style="margin-top: 30px;">
      <el-col :span="12">
        <el-select v-model="projectId" style="width: 200px; display: inline-block;" @change="changeProject">
          <el-option
            v-for="dict in projectNoNameOptions"
            :key="dict.projectId + 'a'"
            :label="dict.title"
            :value="dict.projectId"
          ></el-option>
        </el-select>
        <el-button type="primary" @click="feeShare" style="display: inline-block;margin-left: 10px;">
          费用分摊
        </el-button>
      </el-col>
      <el-col :span="12">
        <el-radio-group v-model="feeTypeName" @change="changeFeeTypeName" style="float: right; margin-right: 30px;">
          <el-radio-button v-for="dict in feeTypeList" :label="dict"></el-radio-button>
        </el-radio-group>
      </el-col>
    </el-row>
    <el-row style="margin-top: 30px;">
      <el-table :data="projectStageUserList" border style="width: 100%" show-summary v-if="feeTypeName=='人员投入'">
        <el-table-column prop="stageName" label="阶段名称"/>
        <el-table-column prop="startDate" label="起始日期"/>
        <el-table-column prop="endDate" label="结束日期"/>
        <el-table-column prop="stageJoinUserName" label="阶段参与人员"/>
        <el-table-column prop="stageHaveUseFee" label="阶段已投入人员费用"/>
        <el-table-column prop="stageRestUseFee" label="阶段可投入人员费用"/>
      </el-table>

      <el-table :data="projectStageMaterialList" border style="width: 100%" show-summary v-if="feeTypeName=='直接投入'">
        <el-table-column prop="stageName" label="阶段名称"/>
        <el-table-column prop="startDate" label="起始日期"/>
        <el-table-column prop="endDate" label="结束日期"/>
        <el-table-column prop="stageHaveUseFee" label="阶段已直接投入费用"/>
        <el-table-column prop="stageRestUseFee" label="阶段可直接投入费用"/>
      </el-table>

      <el-table :data="projectStageDeviceList" border style="width: 100%" show-summary v-if="feeTypeName=='设备折旧'">
        <el-table-column prop="stageName" label="阶段名称"/>
        <el-table-column prop="startDate" label="起始日期"/>
        <el-table-column prop="endDate" label="结束日期"/>
        <el-table-column prop="stageJoinDeviceName" sortable label="阶段投入设备"/>
        <el-table-column prop="stageHaveUseFee" label="阶段已投入设备折旧"/>
        <el-table-column prop="stageRestUseFee" label="阶段可投入设备折旧"/>
      </el-table>

      <el-table :data="projectStageIpList" border style="width: 100%" show-summary v-if="feeTypeName=='无形资产投入'">
        <el-table-column prop="stageName" label="阶段名称"/>
        <el-table-column prop="startDate" label="起始日期"/>
        <el-table-column prop="endDate" label="结束日期"/>
        <el-table-column prop="stageJoinIpName" label="阶段投入无形资产"/>
        <el-table-column prop="stageHaveUseFee" label="阶段无形资产投入"/>
      </el-table>

      <el-table :data="projectStageOtherList" border style="width: 100%" show-summary v-if="feeTypeName=='其他费用投入'">
        <el-table-column prop="stageName" label="阶段名称"/>
        <el-table-column prop="startDate" label="起始日期"/>
        <el-table-column prop="endDate" label="结束日期"/>
        <el-table-column prop="stageHaveUseFee" label="阶段已投入其他费用"/>
        <el-table-column prop="stageRestUseFee" label="阶段可投入其他费用"/>
      </el-table>

      <el-table :data="projectStageDesignList" border style="width: 100%" show-summary v-if="feeTypeName=='新品设计投入'">
        <el-table-column prop="stageName" label="阶段名称"/>
        <el-table-column prop="startDate" label="起始日期"/>
        <el-table-column prop="endDate" label="结束日期"/>
        <el-table-column prop="stageHaveUseFee" label="阶段新品设计投入"/>
        <el-table-column prop="stageRestUseFee" label="阶段可新品设计投入"/>
      </el-table>
    </el-row>

    <!--  配置研发项目初始占比  -->
    <jq-dialog fullscreen :visible.sync="setProjectInitOpen" append-to-body>
      <el-button type="primary" @click="">调整</el-button>
      <el-table :data="projectInitRateList" border style="width: 100%">
        <el-table-column prop="projectName" label="研发项目名称"/>
        <el-table-column prop="startDate" label="起始日期"/>
        <el-table-column prop="endDate" label="结束日期"/>
        <el-table-column prop="userRate" label="人员投入比例">
          <template slot-scope="scope">
            <el-input v-model="scope.row.userRate" @input="checkRate(scope.row)"/>
          </template>
        </el-table-column>
        <el-table-column prop="materialRate" label="直接投入比例">
          <template slot-scope="scope">
            <el-input v-model="scope.row.materialRate" @input="checkRate(scope.row)"/>
          </template>
        </el-table-column>
        <el-table-column prop="deviceRate" label="设备折旧比例">
          <template slot-scope="scope">
            <el-input v-model="scope.row.deviceRate" @input="checkRate(scope.row)"/>
          </template>
        </el-table-column>
        <el-table-column prop="otherRate" label="其他费用投入比例">
          <template slot-scope="scope">
            <el-input v-model="scope.row.otherRate" @input="checkRate(scope.row)"/>
          </template>
        </el-table-column>
        <el-table-column prop="designRate" label="新品设计投入比例">
          <template slot-scope="scope">
            <el-input v-model="scope.row.designRate" @input="checkRate(scope.row)"/>
          </template>
        </el-table-column>
      </el-table>

      <div slot="footer">
        <el-button type="primary" @click="submitProjectRate">确 定</el-button>
        <el-button @click="setProjectInitOpen=false">取 消</el-button>
      </div>
    </jq-dialog>

    <!--  费用分摊  -->
    <jq-dialog fullscreen :visible.sync="feeShareOpen" @close="feeShareOpen = false" append-to-body>
      <project-fee-share ref="feeShareRef" @handleClose="handleClose" :projectId="projectId"
                         :feeTypeName="feeTypeName"/>
      <div slot="footer">
        <el-button type="primary" @click="submitFeeShare">确 定</el-button>
        <el-button @click="handleClose">取 消</el-button>
      </div>
    </jq-dialog>

  </div>
</template>

<script>
import JqTableMixin from '@/mixin/JqTable'
import {
  getProjectByCompanyIdItemNo,
  getProjectInitProportion,
  getProjectInitProportionEcharts,
  getProjectStagePutIntoList,
  getYearProjectPutIntoInfo,
  submitProjectRate
} from '@/api/cost/projectInfo'
import ledgerClassification from '@/views/cost/budgetInfo/common/ledgerClassification.vue'
import ProjectFeeShare from '@/views/cost/project/common/projectFeeShare.vue'

export default {
  name: 'projectFee',
  mixins: [JqTableMixin],
  provide() {
    return {
      handleQuery: this.handleQuery
    }
  },
  data() {
    return {
      companyId: '',
      itemNo: '',
      feeTypeName: '人员投入',
      //人员投入
      projectStageUserList: [],
      //直接投入
      projectStageMaterialList: [],
      //设备折旧投入
      projectStageDeviceList: [],
      //无形资产投入
      projectStageIpList: [],
      //新品设计投入
      projectStageDesignList: [],
      //其他费用投入
      projectStageOtherList: [],
      feeTypeList: ['人员投入', '直接投入', '设备折旧', '无形资产投入', '其他费用投入', '新品设计投入'],
      //项目编号名称下拉数据
      projectNoNameOptions: [],
      //选中的项目
      projectId: '',
      //费用分摊弹框显示
      feeShareOpen: false,

      //配置研发项目初始化占比弹框显示
      setProjectInitOpen: false,
      //研发项目初始比例配置数组
      projectInitRateList: []
    }
  },
  components: { ProjectFeeShare, ledgerClassification },
  watch: {
    '$store.state.item.itemNo'(newVal, oldVal) {
      this.$nextTick(() => {
        this.initProjectAmortizationChart()
        this.initProjectPutIntoChart()
        this.getProjectNoNameSelect()
      })
    }
  },
  created() {
    this.$nextTick(() => {
      if (this.$store.state.item.companyId && this.$store.state.item.itemNo) {
        this.initProjectAmortizationChart()
        this.initProjectPutIntoChart()
        this.getProjectNoNameSelect()
      }
    })
  },
  methods: {

    /** 配置研发项目初始化占比 */
    setProjectInitProportion() {
      this.setProjectInitOpen = true
      const data = {
        companyId: this.$store.state.item.companyId,
        itemNo: this.$store.state.item.itemNo
      }
      getProjectInitProportion(data).then(res => {
        this.projectInitRateList = res.data
      })

    },

    /** 比例校验 */
    checkRate(row) {
      let userRateTotal = 0
      this.projectInitRateList.forEach(item => {
        userRateTotal += Number(item.userRate)
      })
      if (userRateTotal > 100) {
        this.msgError('人员投入比例总和不能超过100')
        row.userRate = 0
        return false
      }

    },

    /** 研发项目初始占比提交 */
    submitProjectRate() {
      const data = {
        companyId: this.$store.state.item.companyId,
        itemNo: this.$store.state.item.itemNo,
        projectRateList: this.projectInitRateList
      }
      submitProjectRate(data).then(res => {
        this.msgSuccess(res.msg)
        this.setProjectInitOpen = false
      })
    },

    /** 获取研发项目摊销占比数据 */
    initProjectAmortizationChart() {
      const that = this
      //等dom加载完毕在获取
      this.$nextTick(() => {
        let myChart = this.$echarts.init(this.$refs.projectAmortizationChart)
        const data = {
          companyId: this.$store.state.item.companyId,
          itemNo: this.$store.state.item.itemNo
        }
        //获取研发项目摊销情况
        getProjectInitProportionEcharts(data).then(res => {
          that.projectList = res.data
          let projectNoList = []
          let series = []
          if (that.projectList) {
            that.projectList.forEach(function(item, i) {
              projectNoList.push(item['projectNo'])
              series.push({
                name: item['projectNo'],
                type: 'bar',
                stack: '总量',
                label: {
                  show: true,
                  position: 'insideRight'
                },
                data: [item['userRate'], item['materialRate'], item['deviceRate'], item['otherRate'], item['designRate']]
              })
            })
            myChart.setOption({
              tooltip: {
                trigger: 'axis',
                axisPointer: {            // 坐标轴指示器，坐标轴触发有效
                  type: 'shadow'        // 默认为直线，可选为：'line' | 'shadow'
                }
              },
              legend: {
                left: '20',
                data: projectNoList
              },
              grid: {
                left: '3%',
                right: '4%',
                bottom: '3%',
                containLabel: true
              },
              xAxis: {
                type: 'value'
              },
              yAxis: {
                type: 'category',
                data: ['人员投入', '直接投入', '设备折旧', '其他费用投入', '新品设计投入']
              },
              series: series
            })
          }
        })
      })
    },

    /** 获取研发项目费用投入情况 */
    initProjectPutIntoChart() {
      const companyId = this.$store.state.item.companyId
      const itemNo = this.$store.state.item.itemNo
      //等dom加载完毕在获取
      this.$nextTick(() => {
        let myChart = this.$echarts.init(this.$refs.projectPutIntoChart)
        //获取客户项目下的研发项目信息
        const params = {
          companyId: companyId,
          itemNo: itemNo
        }
        getYearProjectPutIntoInfo(params).then(res => {
          let projectNoList = []
          let series = []
          for (const key in res.data) {
            projectNoList.push(key)
            let feeTotal = []
            const projectFeeList = res.data[key]
            projectFeeList.forEach(item => {
              feeTotal.push(item.putIntoFeeTotal == 0 ? null : item.putIntoFeeTotal)
            })
            series.push({
              name: key,
              type: 'line',
              stack: 'Total',
              areaStyle: {},
              emphasis: {
                focus: 'series'
              },
              data: feeTotal
            })
          }
          if (projectNoList) {
            myChart.setOption({
              tooltip: {
                trigger: 'axis',
                axisPointer: {
                  type: 'cross',
                  label: {
                    backgroundColor: '#00FF50FF'
                  }
                }
              },
              legend: {
                left: '20',
                data: projectNoList
              },
              grid: {
                left: '3%',
                right: '4%',
                bottom: '3%',
                containLabel: true
              },
              xAxis: [
                {
                  type: 'category',
                  boundaryGap: false,
                  data: ['1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12']
                }
              ],
              yAxis: [
                {
                  type: 'value'
                }
              ],
              series: series
            })
          }
        })
      })
    },

    /** 获取研发项目编号名称下拉数据 */
    getProjectNoNameSelect() {
      const companyId = this.$store.state.item.companyId
      const itemNo = this.$store.state.item.itemNo
      const that = this
      getProjectByCompanyIdItemNo(companyId, itemNo).then(res => {
        res.data.forEach((item, index) => {
          if (index == 0) {
            that.projectId = item.id
          }
          that.projectNoNameOptions.push({
            projectId: item.id,
            title: item.projectNo + '-' + item.projectName
          })
        })

        that.getProjectStagePutIntoList(that.projectId, that.feeTypeName)
      })
    },

    /** 获取项目阶段投入费用列表数据 */
    getProjectStagePutIntoList(projectId, feeTypeName) {
      getProjectStagePutIntoList(projectId, feeTypeName).then(res => {
        if (feeTypeName == '人员投入') {
          this.projectStageUserList = res.data
        } else if (feeTypeName == '直接投入') {
          this.projectStageMaterialList = res.data
        } else if (feeTypeName == '设备折旧') {
          this.projectStageDeviceList = res.data
        } else if (feeTypeName == '无形资产投入') {
          this.projectStageIpList = res.data
        } else if (feeTypeName == '其他费用投入') {
          this.projectStageOtherList = res.data
        } else if (feeTypeName == '新品设计投入') {
          this.projectStageDesignList = res.data
        }
      })
    },

    /** 项目change事件 */
    changeProject(val) {
      this.getProjectStagePutIntoList(val, this.feeTypeName)
    },

    /** 费用类型change事件 */
    changeFeeTypeName(feeTypeName) {
      this.getProjectStagePutIntoList(this.projectId, feeTypeName)
    },

    /** 费用分摊 */
    feeShare() {
      this.feeShareOpen = true
    },

    /** 关闭费用分摊弹框 */
    handleClose() {
      this.feeShareOpen = false
      this.getProjectStagePutIntoList(this.projectId, this.feeTypeName)
      this.initProjectPutIntoChart()
      this.initProjectAmortizationChart()
    },

    /** 提交费用分摊数据 */
    submitFeeShare() {
      this.$refs.feeShareRef.submitFeeShare()
    }
  }
}
</script>
<style lang="scss" scoped>
.el-divider--vertical {
  height: 250px;
  width: 2px;
}

.el-divider {
  background-color: #9398a3;
}
</style>
