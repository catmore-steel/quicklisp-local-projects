<template>
  <jq-card :show="cardShow" :cardKey="cardKey" :title="title" :tags="tags" @handleClose="handleClose">
    <template slot="new">
      <div style="text-align: left;">
          <div>1、标记 * 为必须输入表单。</div>
      </div>
    </template>
    <template slot="operate" v-if="cardKey">
      <!-- <jq-check-user :userIds="[projectInfoForm.dealUserId]">-->
        <el-button icon="el-icon-edit" type="success" @click="handleUpdate"
                   v-hasPermi="['cost:projectInfo:edit']">
          修改
        </el-button>
        <el-button type="danger" icon="el-icon-delete" @click="handleRevoke()"
                 v-hasPermi="['cost:projectInfo:remove']">
          删除
        </el-button>
      <!--</jq-check-user>-->
    </template>
    <template>
      <el-tabs v-model="activeName">
        <el-tab-pane label="研发项目详情" name="1">
            <projectInfo-update :projectInfoId="projectInfoId" :disabled.sync="disabled" @handleClose="handleClose"/>
        </el-tab-pane>
        <template v-if="value">
            <el-tab-pane label="项目预算" name="2" lazy>
              <project-budget-tab :projectInfoId="projectInfoId"/>
            </el-tab-pane>
            <el-tab-pane label="项目阶段" name="3" lazy>
              <project-stages-tab :projectInfoId="projectInfoId"/>
            </el-tab-pane>
            <el-tab-pane label="项目投入" name="4" lazy>
              <project-put-into-tab :projectInfoId="projectInfoId"/>
            </el-tab-pane>
            <el-tab-pane label="项目附件" name="5" lazy>
              <project-attach-tab :projectInfoId="projectInfoId"/>
            </el-tab-pane>
        </template>
      </el-tabs>
    </template>
  </jq-card>
</template>

<script>
  import projectInfoUpdate from './projectInfoUpdate'
  import {isNullOrEmpty} from '@/utils/jq'
  import { delProjectInfo, getProjectInfo, getProjectInfoByCompanyIdItemNoProjectNo } from '@/api/cost/projectInfo'
  import { getPatentInfoByCompanyIdItemNoPatentCode } from '@/api/cost/patentInfo'
  import ProjectStagesTab from '@/views/cost/project/common/tab/projectStagesTab.vue'
  import ProjectBudgetTab from '@/views/cost/project/common/tab/projectBudgetTab.vue'
  import ProjectPutIntoTab from '@/views/cost/project/common/tab/projectPutIntoTab.vue'
  import ProjectAttachTab from '@/views/cost/project/common/tab/projectAttachTab.vue'

  export default {
    name: 'projectInfoDetail',
    components: {
      ProjectAttachTab,
      ProjectPutIntoTab,
      ProjectBudgetTab,
      ProjectStagesTab,
      projectInfoUpdate,
    },
    provide() {
      return {
        handleComplete: this.handleComplete
      }
    },
    inject: ['handleQuery'],
    data() {
      return {
        title: '',
        projectInfoForm: {},
        activeName: '1',
        cardShow: false,
        cardKey: null,
        tags: [],
        projectInfoId: null,
        edit: false,  //开启编辑
        disabled: false,
        projectNo: '',//项目编号
      }
    },
    props: {
      show: {
        type: Boolean,
        default: false
      },
      value: {
        type: String
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

      // 获取研发项目管理详情
      getDetail() {
        let projectNo = this.value
        if (isNullOrEmpty(projectNo)) {
          //新增
          this.title = '新增研发项目管理'
          this.projectNo = null
          this.projectInfoId = null
          this.tags = null
        } else {
          const data = {
            companyId: this.$store.state.item.companyId,
            itemNo: this.$store.state.item.itemNo,
            projectNo: projectNo
          }
          getProjectInfoByCompanyIdItemNoProjectNo(data).then(res => {
            let data = res.data
            this.projectInfoForm = res.data
            this.title = '研发项目管理详情'
            this.projectInfoId = data.id
            this.projectNo = data.projectNo
            this.tags = [{'研发项目编号': data.projectNo}, {'研发项目名称': data.projectName}, {'研发形式': data.rdFormName}, {'研发总投入': data.putIntoFeeTotal}]
          });
        }
      },

      /** 撤销 */
      handleRevoke() {
        const ids = this.projectInfoId;
        this.$confirm('是否确认撤销研发项目管理编号为"' + this.projectNo + '"的数据项?', "警告", {
          confirmButtonText: "确定",
          cancelButtonText: "取消",
          type: "warning"
        }).then(function () {
          return delProjectInfo(ids);
        }).then(() => {
          this.msgSuccess("删除成功");
          this.$emit('handleQuery')
          this.handleClose()
        }).catch(() => {
        })
      },
    }
  }
</script>
