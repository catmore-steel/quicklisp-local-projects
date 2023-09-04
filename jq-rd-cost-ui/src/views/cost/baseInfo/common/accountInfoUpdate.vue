<template>
  <div>
    <el-form ref="detail" :model="detail" :rules="rules" label-width="100px" :disabled="disabled">
      <el-row>
        <el-col :span="8">
          <el-form-item label="所属客户" prop="companyId">
            <jq-select-company :companyId.sync = "detail.companyId"></jq-select-company>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="所属项目" prop="itemNo">
            <jq-select-item :itemNo.sync = "detail.itemNo"></jq-select-item>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="序时账编号" prop="accountNo">
            <el-input v-model="detail.accountNo" placeholder="自动生成序时账编号" disabled/>
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="8">
          <el-form-item label="日期" prop="accDate">
            <el-date-picker
              clearable size="small"
              v-model="detail.accDate"
              class="form-control"
              type="date"
              value-format="yyyy-MM-dd"
              placeholder="选择日期">
            </el-date-picker>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="凭证号" prop="credentialsNo">
            <el-input v-model="detail.credentialsNo" placeholder="请输入凭证号" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="摘要" prop="summary">
            <el-input v-model="detail.summary" placeholder="请输入摘要" />
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="8">
          <el-form-item label="会计科目" prop="accountSubject">
            <el-input v-model="detail.accountSubject" placeholder="请输入会计科目" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="借方金额" prop="debitAmount">
            <el-input v-model="detail.debitAmount" placeholder="请输入借方金额" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="贷方金额" prop="creditAmount">
            <el-input v-model="detail.creditAmount" placeholder="请输入贷方金额" />
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="8">
          <el-form-item label="科目编码" prop="subjectCode">
            <el-input v-model="detail.subjectCode" placeholder="请输入科目编码" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="科目名称" prop="subjectName">
            <el-input v-model="detail.subjectName" placeholder="请输入科目名称" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="会计年度" prop="materialNo">
            <el-input v-model="detail.materialNo" placeholder="请输入会计年度" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="会计期间" prop="materialName">
            <el-input v-model="detail.materialName" placeholder="请输入会计期间" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="顺序号" prop="seqNo">
            <el-input v-model="detail.seqNo" placeholder="请输入顺序号" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="分录号" prop="entryNo">
            <el-input v-model="detail.entryNo" placeholder="请输入分录号" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="凭证字" prop="credentials">
            <el-input v-model="detail.credentials" placeholder="请输入凭证字" />
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
  import {isNullOrEmpty} from '@/utils/jq'
  import {getAccountInfo, delAccountInfo, addAccountInfo, updateAccountInfo} from "@/api/cost/accountInfo";

  export default {
    name: 'accountInfoUpdate',
    components: {},
    inject: ['handleQuery'],
    data() {
      return {
        detail: {},
        //表单校验
        rules: {
          accDate: [
            { required: true, message: "日期不能为空", trigger: "blur" }
          ],          credentialsNo: [
            { required: true, message: "凭证号不能为空", trigger: "blur" }
          ],          summary: [
            { required: true, message: "摘要不能为空", trigger: "blur" }
          ],          accountSubject: [
            { required: true, message: "会计科目不能为空", trigger: "blur" }
          ],
                      subjectCode: [
            { required: true, message: "科目编码不能为空", trigger: "blur" }
          ],          subjectName: [
            { required: true, message: "科目名称不能为空", trigger: "blur" }
          ],
          debitAmount: [
            { required: true, message: "借方金额不能为空", trigger: "blur" }
          ]
        }
      }
    },
    props: {
      accountInfoId: {
        type: Number,
      },
      disabled: {
        type: Boolean,
        require: true
      }
    },
    watch: {
      accountInfoId(val) {
        if (isNullOrEmpty(val)) {
          this.reset()
        } else {
          this.getDetail(val)
        }
      }
    },
    created() {
      this.getDetail(this.accountInfoId)
    },
    methods: {
      /** 表单重置 */
      reset() {
        this.detail = {
          //附件列表
          attachmentList: [],
          id: null,
          accountNo : null,
          materialNo: null,
          materialName: null,
          accDate: null,
          seqNo: null,
          entryNo: null,
          credentials: null,
          credentialsNo: null,
          summary: null,
          accountSubject: null,
          debitAmount: null,
          creditAmount: null,
          subjectCode: null,
          subjectName: null,
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
        this.resetForm("detail");
      },

      /** 获取原始序时账信息详情 */
      getDetail(accountInfoId) {
        if (isNullOrEmpty(accountInfoId)) {
          this.reset()
        } else {
          getAccountInfo(accountInfoId).then(response => {
            this.detail = response.data
          });
        }
      },

      /** 提交按钮 */
      submitForm() {
        this.$refs["detail"].validate(valid => {
          if (valid) {
            if (this.detail.id != null) {
              updateAccountInfo(this.detail).then(response => {
                if (response.code === 200) {
                  this.msgSuccess(response.msg)
                  this.handleClose()
                  this.handleQuery()
                }
              });
            } else {
              addAccountInfo(this.detail).then(response => {
                if (response.code === 200) {
                  this.msgSuccess(response.msg)
                  this.handleClose()
                  this.handleQuery()
                }

              });
            }
          }
        });
      },

      /** 取消按钮 */
      cancel() {
        if (this.accountInfoId) {
          this.getDetail(this.accountInfoId)
          this.$emit('update:disabled', true)
        }
      },

      /** 关闭按钮 */
      handleClose() {
        this.$emit('handleClose')
      },

    }
  }
</script>
