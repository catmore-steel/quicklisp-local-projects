<template>
  <el-table :data="projectStagesList" border show-summary style="width: 100%" >
    <el-table-column prop="stageName" label="阶段名称"/>
    <el-table-column prop="startDate" label="阶段开始时间" width="150" s/>
    <el-table-column prop="endDate" label="阶段结束时间" width="150"/>
    <el-table-column prop="stageJoinUserName" label="参与人员" width="150"/>
    <el-table-column prop="totalDevFee" label="阶段投入合计" width="150"/>
    <el-table-column prop="userDevFee" label="人员投入（元）" width="150"/>
    <el-table-column prop="deviceDevFee" label="设备折旧（元）" width="150"/>
    <el-table-column prop="materialDevFee" label="材料投入（元）" width="150"/>
    <el-table-column prop="ipDevFee" label="无形资产投入（元）" width="180"/>
    <el-table-column prop="fuelPowerDevFee" label="燃料和动力投入（元）" width="200"/>
    <el-table-column prop="otherDevFee" label="其他费用（元）" width="150"/>
  </el-table>
</template>

<script>

import { getProjectStagesList } from '@/api/cost/projectInfo'

export default {
  name: 'projectStagesTab',
  components: { },
  inject: ['handleQuery'],
  data() {
    return {
      //研发项目阶段数据
      projectStagesList: [],
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
      this.getProjectStages()
    }
  },
  created() {
    this.getProjectStages()
  },
  mounted() {
  },
  methods: {

    /** 获取研发项目阶段数据 */
    getProjectStages(){
      getProjectStagesList(this.projectInfoId).then(res=>{
        this.projectStagesList = res.data
      })
    }
  }
}
</script>
