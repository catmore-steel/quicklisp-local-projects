<template>
  <div>
    <el-tabs tab-position="left" v-model="feeTypeName" @tab-click="handleClick">
      <el-tab-pane label="人员投入" name="人员投入">
        <el-row style="margin-bottom: 20px;">
          <el-col :span="8">
            <span>项目阶段：</span>
            <div style="display: inline-block">
              <el-select v-model="projectStage" placeholder="请选择项目阶段" clearable filterable @change="changeStage">
                <el-option
                  v-for="dict in projectStagesOptions"
                  :key="dict.id"
                  :label="dict.stageName"
                  :value="dict.id"
                ></el-option>
              </el-select>
            </div>
          </el-col>
        </el-row>
        <el-row style="margin-bottom: 20px;">
          <span>
            累计人员投入费用：{{ userDevFeeTotal }}元
          </span>
        </el-row>
        <el-table :data="projectStageUserList" border style="width: 100%">
          <el-table-column prop="indexNum" label="序号" width="80"/>
          <el-table-column prop="userName" label="姓名" width="100"/>
          <el-table-column prop="userTypeName" label="人员类型" width="120"/>
          <el-table-column prop="deptName" label="部门" width="100"/>
          <el-table-column prop="stageName" label="阶段名称" width="150"/>
          <el-table-column prop="month" label="参与月度" width="120"/>
          <el-table-column prop="devHours" label="投入工时" width="120"/>
          <el-table-column prop="devSalaryFee" label="当月投入人员费用（工资）" width="220"/>
          <el-table-column prop="devSocialFee" label="当月投入人员费用（社保）" width="220"/>
          <el-table-column prop="devProvidentFee" label="当月投入人员费用（公积金）" width="240"/>
        </el-table>
      </el-tab-pane>

      <el-tab-pane label="直接投入" name="直接投入">
        <el-row style="margin-bottom: 20px;">
          <el-col :span="8">
            <span>项目阶段：</span>
            <div style="display: inline-block">
              <el-select v-model="projectStage" placeholder="请选择项目阶段" clearable filterable @change="changeStage">
                <el-option
                  v-for="dict in projectStagesOptions"
                  :key="dict.id"
                  :label="dict.stageName"
                  :value="dict.id"
                ></el-option>
              </el-select>
            </div>
          </el-col>
        </el-row>
        <el-row style="margin-bottom: 20px;">
          <span>
            累计直接投入费用：{{ materialDevFeeTotal }}元
          </span>
        </el-row>
        <el-table :data="projectStageMaterialList" border style="width: 100%">
          <el-table-column prop="indexNum" label="序号" width="80"/>
          <el-table-column prop="month" label="投入月度" width="120"/>
          <el-table-column prop="stageName" label="项目阶段"/>
          <el-table-column prop="devMaterialFee" label="本次投入物料费用" width="150"/>
          <el-table-column prop="devFuelFee" label="本次投入燃料费用" width="150"/>
          <el-table-column prop="devPowerFee" label="本次投入动力费用" width="150"/>
          <el-table-column prop="devFeeTotal" label="合计直接投入" width="150"/>
        </el-table>
      </el-tab-pane>

      <el-tab-pane label="设备折旧" name="设备折旧">
        <el-row style="margin-bottom: 20px;">
          <el-col :span="8">
            <span>项目阶段：</span>
            <div style="display: inline-block">
              <el-select v-model="projectStage" placeholder="请选择项目阶段" clearable filterable @change="changeStage">
                <el-option
                  v-for="dict in projectStagesOptions"
                  :key="dict.id"
                  :label="dict.stageName"
                  :value="dict.id"
                ></el-option>
              </el-select>
            </div>
          </el-col>
        </el-row>
        <el-row style="margin-bottom: 20px;">
          <span>
            累计折旧费用：{{ deviceDevFeeTotal }}元
          </span>
        </el-row>
        <el-table :data="projectStageDeviceList" border style="width: 100%">
          <el-table-column prop="indexNum" label="序号"/>
          <el-table-column prop="deviceName" label="设备名称"/>
          <el-table-column prop="deviceTypeName" label="设备类型"/>
          <el-table-column prop="stageName" label="项目阶段"/>
          <el-table-column prop="month" label="参与月度"/>
          <el-table-column prop="devHours" label="本次投入工时"/>
          <el-table-column prop="devFee" label="本次设备折旧"/>
        </el-table>
      </el-tab-pane>

      <el-tab-pane label="无形资产投入" name="无形资产投入">
        <el-row style="margin-bottom: 20px;">
          <el-col :span="8">
            <span>项目阶段：</span>
            <div style="display: inline-block">
              <el-select v-model="projectStage" placeholder="请选择项目阶段" clearable filterable @change="changeStage">
                <el-option
                  v-for="dict in projectStagesOptions"
                  :key="dict.id"
                  :label="dict.stageName"
                  :value="dict.id"
                ></el-option>
              </el-select>
            </div>
          </el-col>
        </el-row>
        <el-row style="margin-bottom: 20px;">
          <span>
            累计无形资产摊销费用：{{ ipDevFeeTotal }}元
          </span>
        </el-row>
        <el-table :data="projectStageIpList" border style="width: 100%">
          <el-table-column prop="indexNum" label="序号"/>
          <el-table-column prop="ipName" label="资产名称"/>
          <el-table-column prop="ipTypeName" label="资产类型"/>
          <el-table-column prop="stageName" label="项目阶段"/>
          <el-table-column prop="month" label="投入月份"/>
          <el-table-column prop="devFee" label="本次无形资产摊销费用"/>
        </el-table>
      </el-tab-pane>

      <el-tab-pane label="其他费用投入" name="其他费用投入">
        <el-row style="margin-bottom: 20px;">
          <el-col :span="8">
            <span>项目阶段：</span>
            <div style="display: inline-block">
              <el-select v-model="projectStage" placeholder="请选择项目阶段" clearable filterable @change="changeStage">
                <el-option
                  v-for="dict in projectStagesOptions"
                  :key="dict.id"
                  :label="dict.stageName"
                  :value="dict.id"
                ></el-option>
              </el-select>
            </div>
          </el-col>
        </el-row>
        <el-row style="margin-bottom: 20px;">
          <span>
            累计其他费用：{{otherDevFeeTotal}}元
          </span>
        </el-row>
        <el-table :data="projectStageOtherList" border style="width: 100%">
          <el-table-column prop="indexNum" label="序号"/>
          <el-table-column prop="month" label="投入月度"/>
          <el-table-column prop="stageName" label="项目阶段"/>
          <el-table-column prop="devFee" label="本次投入其他费用"/>
        </el-table>
      </el-tab-pane>

      <el-tab-pane label="新品设计投入" name="新品设计投入">
        <el-row style="margin-bottom: 20px;">
          <el-col :span="8">
            <span>项目阶段：</span>
            <div style="display: inline-block">
              <el-select v-model="projectStage" placeholder="请选择项目阶段" clearable filterable @change="changeStage">
                <el-option
                  v-for="dict in projectStagesOptions"
                  :key="dict.id"
                  :label="dict.stageName"
                  :value="dict.id"
                ></el-option>
              </el-select>
            </div>
          </el-col>
        </el-row>
        <el-row style="margin-bottom: 20px;">
          <span>
            累计投入费用：{{designDevFeeTotal}}元
          </span>
        </el-row>
        <el-table :data="projectStageDesignList" border style="width: 100%">
          <el-table-column prop="indexNum" label="序号" width="80"/>
          <el-table-column prop="month" label="投入月度" width="120"/>
          <el-table-column prop="stageName" label="项目阶段" width="150"/>
          <el-table-column prop="devFee" label="新品设计费" width="150"/>
          <el-table-column prop="devFeeTwo" label="新工艺规程制定费" width="150"/>
          <el-table-column prop="devFeeThree" label="新药研制的临床试验费" width="170"/>
          <el-table-column prop="devFeeFour" label="勘探开发技术的现场试验费" width="200"/>
          <el-table-column prop="devFeeTotal" label="合计"/>
        </el-table>
      </el-tab-pane>
    </el-tabs>
  </div>

</template>

<script>

import { getProjectStagesPutIntoDetailList, getProjectStagesSelect } from '@/api/cost/projectInfo'

export default {
  name: 'projectPutIntoTab',
  components: {},
  inject: ['handleQuery'],
  data() {
    return {
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

      //累计人员投入费用
      userDevFeeTotal: null,
      //累计直接投入费用
      materialDevFeeTotal: null,
      //累计设备折旧投入费用
      deviceDevFeeTotal: null,
      //累计无形资产投入费用
      ipDevFeeTotal: null,
      //累计新品设计投入费用
      designDevFeeTotal: null,
      //累计其他投入费用
      otherDevFeeTotal: null,

      //项目阶段下拉数据列表
      projectStagesOptions: [],
      //选中的项目阶段
      projectStage: '',
      //费用类型投入名称选择数据
      feeTypeName: '人员投入'
    }
  },
  props: {
    projectInfoId: {
      type: Number
    }
  },
  watch: {
    projectInfoId(val) {
      this.projectInfoId = val
      this.feeTypeName = '人员投入'
      this.getProjectStages()
      this.getProjectStagesSelect()
    }
  },
  created() {
    this.getProjectStages()
    this.getProjectStagesSelect()
  },
  mounted() {
  },
  methods: {

    /** 获取研发项目阶段下拉 */
    getProjectStagesSelect() {
      getProjectStagesSelect(this.projectInfoId).then(res => {
        this.projectStagesOptions = res.data
      })
    },

    /** tab切换事件 */
    handleClick(tab, event) {
      this.feeTypeName = tab.name
      this.getProjectStages()
    },

    /** 项目阶段切换事件 */
    changeStage(val) {
      this.projectStage = val
      this.getProjectStages()
    },

    /** 获取研发项目阶段数据 */
    getProjectStages() {
      getProjectStagesPutIntoDetailList(this.projectInfoId, this.feeTypeName, this.projectStage).then(res => {
        if (this.feeTypeName == '人员投入') {
          this.projectStageUserList = res.data
          this.userDevFeeTotal = 0
          this.projectStageUserList.forEach(item => {
            this.userDevFeeTotal += item.devSalaryFee
            this.userDevFeeTotal += item.devSocialFee
            this.userDevFeeTotal += item.devProvidentFee
          })
          this.userDevFeeTotal = this.userDevFeeTotal.toFixed(2)

        } else if (this.feeTypeName == '直接投入') {
          this.projectStageMaterialList = res.data
          this.materialDevFeeTotal = 0
          this.projectStageMaterialList.forEach(item => {
            this.materialDevFeeTotal += item.devMaterialFee
            this.materialDevFeeTotal += item.devFuelFee
            this.materialDevFeeTotal += item.devPowerFee
          })
          this.materialDevFeeTotal = this.materialDevFeeTotal.toFixed(2)

        } else if (this.feeTypeName == '设备折旧') {
          this.projectStageDeviceList = res.data
          this.deviceDevFeeTotal = 0
          this.projectStageDeviceList.forEach(item => {
            this.deviceDevFeeTotal += item.devFee
          })
          this.deviceDevFeeTotal = this.deviceDevFeeTotal.toFixed(2)

        } else if (this.feeTypeName == '无形资产投入') {
          this.projectStageIpList = res.data
          this.ipDevFeeTotal = 0
          this.projectStageIpList.forEach(item => {
            this.ipDevFeeTotal += item.devFee
          })
          this.ipDevFeeTotal = this.ipDevFeeTotal.toFixed(2)

        } else if (this.feeTypeName == '其他费用投入') {
          this.projectStageOtherList = res.data
          this.otherDevFeeTotal = 0
          this.projectStageOtherList.forEach(item => {
            this.otherDevFeeTotal += item.devFee
          })
          this.otherDevFeeTotal = this.otherDevFeeTotal.toFixed(2)

        } else if (this.feeTypeName == '新品设计投入') {
          this.projectStageDesignList = res.data
          this.designDevFeeTotal = 0
          this.projectStageDesignList.forEach(item => {
            this.designDevFeeTotal += item.devFee
            this.designDevFeeTotal += item.devFeeTwo
            this.designDevFeeTotal += item.devFeeThree
            this.designDevFeeTotal += item.devFeeFour
          })
          this.designDevFeeTotal = this.designDevFeeTotal.toFixed(2)

        }
      })
    }
  }
}
</script>
