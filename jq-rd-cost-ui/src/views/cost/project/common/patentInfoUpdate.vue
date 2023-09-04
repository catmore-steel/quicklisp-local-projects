<template>
  <div>
    <el-form ref="detail" :model="detail" :rules="rules" label-width="100px" :disabled="disabled">
      <jq-panel title="基本信息">
        <el-row>
          <el-col :span="8">
            <el-form-item label="知产编号" prop="patentCode">
              <el-input v-model="detail.patentCode" placeholder="请输入知产编号"/>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="知产名称" prop="patentName">
              <el-input v-model="detail.patentName" placeholder="请输入知产名称"/>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="申请日期" prop="applyDate">
              <el-date-picker
                clearable size="small"
                v-model="detail.applyDate"
                class="form-control"
                type="date"
                value-format="yyyy-MM-dd"
                placeholder="选择申请日期">
              </el-date-picker>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="8">
            <el-form-item label="知产类别" prop="patentType">
              <el-select v-model="detail.patentType" placeholder="请选择知产类别">
                <el-option
                  v-for="dict in patentTypeOptions"
                  :key="dict.id"
                  :label="dict.name"
                  :value="dict.id"
                ></el-option>
              </el-select>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="知产状态" prop="patentStatus">
              <jq-dict-select :value.sync="detail.patentStatus" dict-type="patent_status" placeholder="请选择知产状态"/>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="专利号" prop="patentNo">
              <el-input v-model="detail.patentNo" placeholder="请输入专利号"/>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="8">
            <el-form-item label="授权日期" prop="grantDate">
              <el-date-picker
                clearable size="small"
                v-model="detail.grantDate"
                class="form-control"
                type="date"
                value-format="yyyy-MM-dd"
                placeholder="选择授权日期">
              </el-date-picker>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="获得方式" prop="patentGetType">
              <jq-dict-select :value.sync="detail.patentGetType" dict-type="patent_get_type" placeholder="请选择获得方式"/>
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
import { isNullOrEmpty } from '@/utils/jq'
import { addPatentInfo, getPatentInfo, getPatentTypeSelect, updatePatentInfo } from '@/api/cost/patentInfo'

export default {
  name: 'patentInfoUpdate',
  components: {},
  inject: ['handleQuery'],
  data() {
    return {
      detail: {},
      // 知产类别字典
      patentTypeOptions: [],
      //表单校验
      rules: {
        patentCode: [
          { required: true, message: '知产编号不能为空', trigger: 'blur' }
        ],
        patentName: [
          { required: true, message: '知产名称不能为空', trigger: 'blur' }
        ],
        applyDate: [
          { required: true, message: '申请日期不能为空', trigger: 'blur' }
        ],
        patentType: [
          { required: true, message: '知产类别不能为空', trigger: 'change' }
        ],
        patentStatus: [
          { required: true, message: '知产状态不能为空', trigger: 'change' }
        ]
      }
    }
  },
  props: {
    patentInfoId: {
      type: Number
    },
    disabled: {
      type: Boolean,
      require: true
    }
  },
  watch: {
    patentInfoId(val) {
      if (isNullOrEmpty(val)) {
        this.reset()
      } else {
        this.getDetail(val)
      }
    }
  },
  created() {
    this.getDetail(this.patentInfoId)
    this.getPatentTypes()
  },
  methods: {
    /** 表单重置 */
    reset() {
      this.detail = {
        //附件列表
        attachmentList: [],
        id: null,
        patentCode: null,
        patentName: null,
        patentNo: null,
        patentGetType: null,
        patentStatus: null,
        patentType: null,
        applyDate: null,
        grantDate: null,
        itemNo: null,
        companyId: null,
        tenantId: null,
        revision: null,
        createBy: null,
        createTime: null,
        updateBy: null,
        updateTime: null,
        remark: null,
        delFlag: null
      }
      this.resetForm('detail')
    },

    /** 获取知产类别下拉数据 */
    getPatentTypes() {
      getPatentTypeSelect().then(res => {
        this.patentTypeOptions = res.data
      })
    },

    /** 获取企业知识产权信息详情 */
    getDetail(patentInfoId) {
      if (isNullOrEmpty(patentInfoId)) {
        this.reset()
      } else {
        getPatentInfo(patentInfoId).then(response => {
          this.detail = response.data
        })
      }
    },

    /** 提交按钮 */
    submitForm() {
      this.$refs['detail'].validate(valid => {
        if (valid) {
          this.detail.itemNo = this.$store.state.item.itemNo
          this.detail.companyId = this.$store.state.item.companyId
          if (this.detail.id != null) {
            updatePatentInfo(this.detail).then(response => {
              if (response.code === 200) {
                this.msgSuccess(response.msg)
                this.handleClose()
                this.handleQuery()
              }
            })
          } else {
            addPatentInfo(this.detail).then(response => {
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
      if (this.patentInfoId) {
        this.getDetail(this.patentInfoId)
        this.$emit('update:disabled', true)
      }
    },

    /** 关闭按钮 */
    handleClose() {
      this.$emit('handleClose')
    }

  }
}
</script>
