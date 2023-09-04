<template>
  <div>
    <el-form ref="detail" :model="detail" :rules="rules" label-width="120px" :disabled="disabled">
      <el-row>
        <el-col :span="12">
          <el-form-item label="部门类型" prop="deptType">
            <el-select v-model="detail.deptType" placeholder="请选择部门类型" disabled>
              <el-option
                v-for="dict in deptTypeOptions"
                :key="dict.dictValue"
                :label="dict.dictLabel"
                :value="dict.dictValue"
              ></el-option>
            </el-select>
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="费用项目" prop="feeItem">
            <el-select v-model="detail.feeItem" placeholder="请选择费用项目" disabled>
              <el-option
                v-for="dict in feeItemOptions"
                :key="dict.dictValue"
                :label="dict.dictLabel"
                :value="dict.dictValue"
              ></el-option>
            </el-select>
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="24">
          <el-form-item label="关联会计科目" prop="codesArray">
            <el-select v-model="detail.codesArray" multiple placeholder="请选择关联会计科目">
              <el-option
                v-for="item in accountConfigOptions"
                :key="item.code"
                :label="item.name"
                :value="item.code"
              ></el-option>
            </el-select>
          </el-form-item>
        </el-col>
      </el-row>
      <div class="fixed_coperate" v-show="!disabled">
        <el-button type="primary" @click="submitForm">确 定</el-button>
        <el-button @click="cancel">取 消</el-button>
      </div>
    </el-form>
  </div>
</template>

<script>
import { isNullOrEmpty } from '@/utils/jq'
import { getAccountSplit, delAccountSplit, addAccountSplit, updateAccountSplit } from '@/api/conf/accountSplit'
import { listAccountConfig } from '@/api/conf/confAccountConfig'

export default {
  name: 'accountSplitUpdate',
  components: {},
  // inject: ['handleQuery'],
  data() {
    return {
      detail: {},
      deptTypeOptions: [],
      feeItemOptions: [],
      accountConfigOptions: [],
      //表单校验
      rules: {
        deptType: [
          { required: true, message: '部门类型不能为空', trigger: 'change' }
        ], feeItem: [
          { required: true, message: '费用项目不能为空', trigger: 'change' }
        ], codesArray: [
          { required: true, message: '关联会计科目不能为空', trigger: 'blur' }
        ]
      }
    }
  },
  props: {
    accountSplitId: {
      type: Number
    },
    disabled: {
      type: Boolean,
      require: true
    },
    deptTypeUpdate: {
      type: String
    },
    feeItemUpdate: {
      type: String
    }
  },
  watch: {
    accountSplitId(val) {
      if (isNullOrEmpty(val)) {
        this.reset()
      } else {
        this.accountSplitId = val
        this.getDetail(val)
      }
    },
    deptTypeUpdate(val) {
    },
    feeItemUpdate(val) {
    }
  },
  created() {
    this.getDicts('dept_type').then(response => {
      this.deptTypeOptions = response.data
    })
    this.getDicts('fee_item').then(response => {
      this.feeItemOptions = response.data
    })
    listAccountConfig(null).then(res => {
      this.accountConfigOptions = res.data
    })
    this.getDetail(this.accountSplitId)
  },
  methods: {
    /** 表单重置 */
    reset() {
      this.detail = {
        //附件列表
        attachmentList: [],
        id: null,
        deptType: this.deptTypeUpdate,
        feeItem: this.feeItemUpdate,
        codesArray: [],
        createBy: null,
        createTime: null,
        updateBy: null,
        updateTime: null,
        remark: null,
        delFlag: null
      }
      this.resetForm('detail')
    },

    /** 获取费用拆分配置详情 */
    getDetail(accountSplitId) {
      if (isNullOrEmpty(accountSplitId)) {
        this.reset()
      } else {
        getAccountSplit(accountSplitId).then(response => {
          this.detail = response.data
        })
      }
    },

    /** 提交按钮 */
    submitForm() {
      this.$refs['detail'].validate(valid => {
        if (valid) {
          if (this.detail.id != null) {
            updateAccountSplit(this.detail).then(response => {
              if (response.code === 200) {
                this.msgSuccess(response.msg)
                this.handleClose()
                // this.handleQuery()
              }
            })
          } else {
            addAccountSplit(this.detail).then(response => {
              if (response.code === 200) {
                this.msgSuccess(response.msg)
                this.handleClose()
                // this.handleQuery()
              }

            })
          }
        }
      })
    },

    /** 取消按钮 */
    cancel() {
      if (this.accountSplitId) {
        this.getDetail(this.accountSplitId)
        this.$emit('update:disabled', true)
      } else {
        this.handleClose()
      }
    },

    /** 关闭按钮 */
    handleClose() {
      this.$emit('handleClose')
    }

  }
}
</script>
