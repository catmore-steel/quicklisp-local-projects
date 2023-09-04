<template>
  <div>
    <el-form ref="detail" :model="detail" :rules="rules" label-width="100px" :disabled="disabled">
      <el-row>
        <el-col :span="12">
          <el-form-item label="员工姓名" prop="userName">
            <el-input v-model="detail.userName" placeholder="请输入员工ID" />
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="考勤天数" prop="workingDate">
            <el-input v-model="detail.workingDate" placeholder="请输入考勤天数" />
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="12">
          <el-form-item label="薪酬月份;YYYY-MM" prop="salaryMonth">
            <el-input v-model="detail.salaryMonth" placeholder="请输入薪酬月份;YYYY-MM" />
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="应发工资" prop="salaryFee">
            <el-input v-model="detail.salaryFee" placeholder="请输入应发工资" />
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="12">
          <el-form-item label="社保" prop="safeFee">
            <el-input v-model="detail.safeFee" placeholder="请输入社保" />
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="公积金" prop="goldFee">
            <el-input v-model="detail.goldFee" placeholder="请输入公积金" />
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="12">
          <el-form-item label="所属项目" prop="itemNo">
            <el-input v-model="detail.itemNo" placeholder="请输入所属项目" />
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="所属客户" prop="companyId">
            <el-input v-model="detail.companyId" placeholder="请输入所属客户" />
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="12">
          <el-form-item label="租户号" prop="tenantId">
            <el-input v-model="detail.tenantId" placeholder="请输入租户号" />
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="乐观锁" prop="revision">
            <el-input v-model="detail.revision" placeholder="请输入乐观锁" />
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="12">
          <el-form-item label="备注" prop="remark">
            <el-input v-model="detail.remark" placeholder="请输入备注" />
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="逻辑删除" prop="delFlag">
            <el-input v-model="detail.delFlag" placeholder="请输入逻辑删除" />
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
  import {getUserSalary, delUserSalary, addUserSalary, updateUserSalary} from "@/api/cost/userSalary";

  export default {
    name: 'userSalaryUpdate',
    components: {},
    inject: ['handleQuery'],
    data() {
      return {
        detail: {},
        //表单校验
        rules: {
          userId: [
            { required: true, message: "员工ID不能为空", trigger: "blur" }
          ],          salaryMonth: [
            { required: true, message: "薪酬月份;YYYY-MM不能为空", trigger: "blur" }
          ],          salaryFee: [
            { required: true, message: "应发工资不能为空", trigger: "blur" }
          ],          safeFee: [
            { required: true, message: "社保不能为空", trigger: "blur" }
          ],          goldFee: [
            { required: true, message: "公积金不能为空", trigger: "blur" }
          ],          itemNo: [
            { required: true, message: "所属项目不能为空", trigger: "blur" }
          ],          companyId: [
            { required: true, message: "所属客户不能为空", trigger: "blur" }
          ],          delFlag: [
            { required: true, message: "逻辑删除不能为空", trigger: "blur" }
          ]        }
      }
    },
    props: {
      userSalaryId: {
        type: Number,
      },
      disabled: {
        type: Boolean,
        require: true
      }
    },
    watch: {
      userSalaryId(val) {
        if (isNullOrEmpty(val)) {
          this.reset()
        } else {
          this.getDetail(val)
        }
      }
    },
    created() {
      this.getDetail(this.userSalaryId)
    },
    methods: {
      /** 表单重置 */
      reset() {
        this.detail = {
          //附件列表
          attachmentList: [],
          id: null,
          userId: null,
          workingDate: null,
          salaryMonth: null,
          salaryFee: null,
          safeFee: null,
          goldFee: null,
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

      /** 获取企业人员工资记录;详情 */
      getDetail(userSalaryId) {
        if (isNullOrEmpty(userSalaryId)) {
          this.reset()
        } else {
          getUserSalary(userSalaryId).then(response => {
            this.detail = response.data
          });
        }
      },

      /** 提交按钮 */
      submitForm() {
        this.$refs["detail"].validate(valid => {
          if (valid) {
            if (this.detail.id != null) {
              updateUserSalary(this.detail).then(response => {
                if (response.code === 200) {
                  this.msgSuccess(response.msg)
                  this.handleClose()
                  this.handleQuery()
                }
              });
            } else {
              addUserSalary(this.detail).then(response => {
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
        if (this.userSalaryId) {
          this.getDetail(this.userSalaryId)
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
