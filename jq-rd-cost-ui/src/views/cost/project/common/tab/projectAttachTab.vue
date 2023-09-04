<template>
  <div>
    <el-button type="primary" @click="$refs.file.click()" style="margin-bottom: 20px">上传</el-button>
    <input ref="file" style="display: none;" type="file" @change="uploadRequest($event)">

    <el-table :data="projectAttachList" border show-summary style="width: 100%">
      <el-table-column prop="stageName" label="附件类型"/>
      <el-table-column prop="startDate" label="材料名称">
        <template slot-scope="scope">
          <span @click="">{{scope.row.fielName}}</span>
        </template>
      </el-table-column>
      <el-table-column prop="endDate" label="上传时间"/>
      <el-table-column prop="stageJoinUserName" label="创建人"/>
      <el-table-column prop="totalDevFee" label="操作">
        <template slot-scope="scope">
          <el-button type="danger" icon="el-icon-delete" @click="">
            删除
          </el-button>
        </template>
      </el-table-column>
    </el-table>
  </div>
</template>

<script>

import { getProjectStagesList, uploadProjectAttach } from '@/api/cost/projectInfo'

export default {
  name: 'projectAttachTab',
  components: {},
  inject: ['handleQuery'],
  data() {
    return {
      //研发项目附件数据
      projectAttachList: []
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
      this.getProjectStages(val)
    }
  },
  created() {
    this.getProjectStages()
  },
  mounted() {
  },
  methods: {

    /** 获取研发项目阶段数据 */
    getProjectStages() {
      getProjectStagesList(this.projectInfoId).then(res => {
        this.projectStagesList = res.data
      })
    },

    /** 上传文件 */
    uploadRequest(param) {
      param.preventDefault()
      const files = this.$refs.file.files
      const data = new FormData()
      for (const file of files) {
        data.append('file', file)
      }
      uploadProjectAttach(data).then(res => {
        this.msgSuccess(res.msg)
      }).catch(err => {
        this.msgError(err.msg)
      })
    }
  }
}
</script>
