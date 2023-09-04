<template>
  <div>
    <el-form ref="detail" :model="detail" :rules="rules" label-width="100px" :disabled="disabled">
      <jq-panel title="项目信息">
        <el-row>
          <el-col :span="8">
            <el-form-item label="项目编号" prop="projectNo">
              <el-input v-model="detail.projectNo" placeholder="请输入项目编号"/>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="项目名称" prop="projectName">
              <el-input v-model="detail.projectName" placeholder="请输入项目名称"/>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="研发形式" prop="rdForm">
              <jq-dict-select :value.sync="detail.rdForm" dict-type="rd_form" placeholder="请选择研发形式"/>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="8">
            <el-form-item label="项目负责人" prop="directorUserId">
              <jq-user-select :check-many="false" :agent.sync="detail.directorUserId" placeholder="请选择项目负责人"/>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="技术领域" prop="technicalCode">
              <treeselect v-model="detail.technicalCode" :options="technicalOptions" :normalizer="normalizer" class="form-control"
                          :disable-branch-nodes="true" placeholder="请选择技术领域"
              />
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="立项年度" prop="projectApprovalYear">
              <el-date-picker
                clearable size="small"
                v-model="detail.projectApprovalYear"
                class="form-control"
                type="year"
                placeholder="选择立项年度"
              >
              </el-date-picker>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="8">
            <el-form-item label="开始时间" prop="startDateDay">
              <el-date-picker @change="startDateDayChange()"
                clearable size="small"
                v-model="detail.startDateDay"
                class="form-control"
                type="month"
                placeholder="选择开始时间"
              >
              </el-date-picker>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="结束时间" prop="endDateDay">
              <el-date-picker
                clearable size="small"
                v-model="detail.endDateDay"
                class="form-control"
                type="month"
                placeholder="选择结束时间"
              >
              </el-date-picker>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="总预算" prop="budgetFeeTotal">
              <el-input v-model="detail.budgetFeeTotal" placeholder="预算总计带入（不可修改）" disabled/>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="8">
            <el-form-item label="研发总投入" prop="putIntoFeeTotal">
              <el-input v-model="detail.putIntoFeeTotal" placeholder="投入总计带入（不可修改）" disabled/>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="资本化日期"  prop="capitalizationDate">
              <el-date-picker
                clearable size="small"
                v-model="detail.capitalizationDate"
                class="form-control"
                type="date"
                value-format="yyyy-MM-dd"
                placeholder="选择资本化日期">
              </el-date-picker>
            </el-form-item>
          </el-col>
          <!-- <el-col :span="8">
            <el-form-item label="主要研究内容" prop="mainContent">
              <el-input v-model="detail.mainContent" placeholder="请输入主要研究内容"/>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="预期目标" prop="expectedGoal">
              <el-input v-model="detail.expectedGoal" placeholder="请输入预期目标"/>
            </el-form-item>
          </el-col>-->
        </el-row>
        <el-row>
          <el-col :span="8">
            <el-form-item label="备注" prop="remark">
              <el-input v-model="detail.remark" type="textarea" rows="2" placeholder="请输入备注"/>
            </el-form-item>
          </el-col>
        </el-row>
      </jq-panel>
      <jq-panel title="委托信息">
        <el-row>
          <el-col :span="8">
            <el-form-item label="是否委托研发" prop="isEntrust">
              <el-switch
                style="display: block;"
                v-model="detail.isEntrust"
                active-color="#13ce66"
                inactive-color="#ff4949"
                active-text="不含"
                inactive-text="含"
                active-value="N"
                inactive-value="Y"
              >
              </el-switch>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="8">
            <el-form-item label="委托单位" prop="entrustName">
              <el-input v-model="detail.entrustName" placeholder="请输入委托单位"/>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="是否委托境外" prop="isOverseas">
              <el-switch
                style="display: block;"
                v-model="detail.isOverseas"
                active-color="#13ce66"
                inactive-color="#ff4949"
                active-text="境内"
                inactive-text="境外"
                active-value="N"
                inactive-value="Y"
              >
              </el-switch>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="是否关联" prop="isAssociate">
              <el-switch
                style="display: block;"
                v-model="detail.isAssociate"
                active-color="#13ce66"
                inactive-color="#ff4949"
                active-text="不关联"
                inactive-text="关联"
                active-value="N"
                inactive-value="Y"
              >
              </el-switch>
            </el-form-item>
          </el-col>
        </el-row>
      </jq-panel>
      <div class="fixed_coperate" v-show="!disabled">
        <el-button type="primary" @click="submitForm">确 定</el-button>
        <el-button @click="cancel">取 消</el-button>
      </div>
    </el-form>
  </div>
</template>

<script>
import { dateTypeFormat, isNullOrEmpty } from '@/utils/jq'
import { listField } from '@/api/conf/field'
import { addProjectInfo, getProjectInfo, updateProjectInfo } from '@/api/cost/projectInfo'

export default {
  name: 'projectInfoUpdate',
  components: {},
  inject: ['handleQuery'],
  data() {
    return {
      detail: {},
      //技术领域下拉数据
      technicalOptions: [],
      //研发形式字典
      rdFormOptions: [],
      //表单校验
      rules: {
        projectNo: [
          { required: true, message: '项目编号不能为空', trigger: 'blur' }
        ],
        projectName: [
          { required: true, message: '项目名称不能为空', trigger: 'blur' }
        ],
        rdForm: [
          { required: true, message: '研发形式不能为空', trigger: 'blur' }
        ],
        directorUserId: [
          { required: true, message: '项目负责人不能为空', trigger: 'blur' }
        ],
        startDateDay: [
          { required: true, message: '开始时间不能为空', trigger: 'blur' }
        ],
        endDateDay: [
          { required: true, message: '结束时间不能为空', trigger: 'blur' }
        ],
        // budgetFeeTotal: [
        //   { required: true, message: '总预算不能为空', trigger: 'blur' }
        // ],
        // investmentTotal: [
        //   { required: true, message: '研发总投入不能为空', trigger: 'blur' }
        // ]
      }
    }
  },
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
      if (isNullOrEmpty(val)) {
        this.reset()
      } else {
        this.getDetail(val)
      }
    }
  },
  created() {
    this.getDetail(this.projectInfoId)
    this.getDicts('rd_form').then(response => {
      this.rdFormOptions = response.data
    })
    listField(null).then(response => {
      this.technicalOptions = this.handleTree(response.data, 'codeNumber', 'parentCodeNumber')
    })
    this.detail.companyId = this.$store.state.item.companyId
    this.detail.itemNo = this.$store.state.item.itemNo
  },
  methods: {
    /** 表单重置 */
    reset() {
      this.detail = {
        //附件列表
        attachmentList: [],
        id: null,
        projectNo: null,
        projectName: null,
        rdForm: null,
        technicalSource: null,
        technicalCode: null,
        moneySource: null,
        startDate: null,
        endDate: null,
        directorUserId: null,
        isEntrust: null,
        isAssociate: null,
        isOverseas: null,
        entrustName: null,
        itemNo: null,
        companyId: null,
        tenantId: null,
        revision: null,
        createBy: null,
        createTime: null,
        updateBy: null,
        updateTime: null,
        remark: null,
        delFlag: null,
        projectApprovalYear: null
      }
      this.resetForm('detail')
    },

    /** 转换机构数据结构 */
    normalizer(node) {
      if (node.children && !node.children.length) {
        delete node.children
      }
      return {
        id: node.codeNumber,
        label: node.title,
        children: node.children
      }
    },

    /** 获取研发项目管理详情 */
    getDetail(projectInfoId) {
      if (isNullOrEmpty(projectInfoId)) {
        this.reset()
      } else {
        getProjectInfo(projectInfoId).then(response => {
          this.detail = response.data
          this.detail.projectApprovalYear = this.detail.startDate.split('-')[0]
        })
      }
    },

    /** 提交按钮 */
    submitForm() {
      this.$refs['detail'].validate(valid => {
        if (valid) {
          if (this.detail.id != null) {
            updateProjectInfo(this.detail).then(response => {
              if (response.code === 200) {
                this.msgSuccess(response.msg)
                this.handleClose()
                this.handleQuery()
              }
            })
          } else {
            addProjectInfo(this.detail).then(response => {
              if (response.code === 200) {
                this.msgSuccess(response.msg)
                this.handleClose()
                this.handleQuery()
              }

            })
          }
        }
      })
    },

    /** 取消按钮 */
    cancel() {
      if (this.projectInfoId) {
        this.getDetail(this.projectInfoId)
        this.$emit('update:disabled', true)
      }
    },

    /** 关闭按钮 */
    handleClose() {
      this.$emit('handleClose')
    },

    startDateDayChange() {
      console.log(this.detail.startDateDay)
      this.detail.projectApprovalYear = this.detail.startDateDay
    }
  }
}
</script>
