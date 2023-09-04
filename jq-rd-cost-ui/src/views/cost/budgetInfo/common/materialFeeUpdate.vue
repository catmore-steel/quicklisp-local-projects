<template>
  <div>
    <el-form ref="detail" :model="detail" :rules="rules" label-width="100px" :disabled="disabled">
      <el-row>
        <el-col :span="12">
          <el-form-item label="月度;YYYY-MM" prop="month">
            <el-input v-model="detail.month" placeholder="请输入月度;YYYY-MM" />
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="当月投入研发材料费用" prop="devMaterialFee">
            <el-input v-model="detail.devMaterialFee" placeholder="请输入当月投入研发材料费用" />
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="12">
          <el-form-item label="当月投入研发燃料费用" prop="devFuelFee">
            <el-input v-model="detail.devFuelFee" placeholder="请输入当月投入研发燃料费用" />
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="当月投入研发动力费用" prop="devPowerFee">
            <el-input v-model="detail.devPowerFee" placeholder="请输入当月投入研发动力费用" />
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
  import {getMaterialFee, delMaterialFee, addMaterialFee, updateMaterialFee} from "@/api/cost/materialFee";

  export default {
    name: 'materialFeeUpdate',
    components: {},
    inject: ['handleQuery'],
    data() {
      return {
        detail: {},
        //表单校验
        rules: {
          month: [
            { required: true, message: "月度;YYYY-MM不能为空", trigger: "blur" }
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
      materialFeeId: {
        type: Number,
      },
      disabled: {
        type: Boolean,
        require: true
      }
    },
    watch: {
      materialFeeId(val) {
        if (isNullOrEmpty(val)) {
          this.reset()
        } else {
          this.getDetail(val)
        }
      }
    },
    created() {
      this.getDetail(this.materialFeeId)
    },
    methods: {
      /** 表单重置 */
      reset() {
        this.detail = {
          //附件列表
          attachmentList: [],
          id: null,
          month: null,
          devMaterialFee: null,
          devFuelFee: null,
          devPowerFee: null,
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

      /** 获取直接投入费用拟定;详情 */
      getDetail(materialFeeId) {
        if (isNullOrEmpty(materialFeeId)) {
          this.reset()
        } else {
          getMaterialFee(materialFeeId).then(response => {
            this.detail = response.data
          });
        }
      },

      /** 提交按钮 */
      submitForm() {
        this.$refs["detail"].validate(valid => {
          if (valid) {
            if (this.detail.id != null) {
              updateMaterialFee(this.detail).then(response => {
                if (response.code === 200) {
                  this.msgSuccess(response.msg)
                  this.handleClose()
                  this.handleQuery()
                }
              });
            } else {
              addMaterialFee(this.detail).then(response => {
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
        if (this.materialFeeId) {
          this.getDetail(this.materialFeeId)
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
