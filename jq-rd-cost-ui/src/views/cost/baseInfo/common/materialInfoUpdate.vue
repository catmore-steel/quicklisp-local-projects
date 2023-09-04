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
      </el-row>
      <el-row>
        <el-col :span="8">
          <el-form-item label="物料编号" prop="materialNo">
            <el-input v-model="detail.materialNo" placeholder="自动生成"  disabled/>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="物料名称" prop="materialName">
            <el-input v-model="detail.materialName" placeholder="请输入物料名称" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="含税单价" prop="unitPrice">
            <el-input v-model="detail.unitPrice" placeholder="请输入含税单价" />
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="8">
          <el-form-item label="规格" prop="specs">
            <el-input v-model="detail.specs" placeholder="请输入规格" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="单位" prop="unit">
            <el-input v-model="detail.unit" placeholder="请输入单位" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="单价最近生效时间"  prop="takingTime">
            <el-date-picker
              clearable size="small"
              v-model="detail.takingTime"
              class="form-control"
              type="date"
              value-format="yyyy-MM-dd"
              placeholder="选择单价最近生效时间">
            </el-date-picker>
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="24">
          <el-form-item label="备注" prop="remark">
            <el-input type="textarea" rows="10"   v-model="detail.remark" placeholder="请输入备注" />
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
  import {getMaterialInfo, delMaterialInfo, addMaterialInfo, updateMaterialInfo} from "@/api/cost/materialInfo";

  export default {
    name: 'materialInfoUpdate',
    components: {},
    inject: ['handleQuery'],
    data() {
      return {
        detail: {},
        companyId: null,
        //表单校验
        rules: {
          companyId: [
            { required: true, message: "客户名称不能为空", trigger: "blur" }
          ],
          itemNo: [
            { required: true, message: "所属项目不能为空", trigger: "blur" }
          ],
                  materialName: [
            { required: true, message: "材料名称不能为空", trigger: "blur" }
          ],          specs: [
            { required: true, message: "规格不能为空", trigger: "blur" }
          ],
          unit: [
            { required: true, message: "单位不能为空", trigger: "blur" }
          ],
        unitPrice: [
          { required: true, message: "含税单价不能为空", trigger: "blur" }
        ],
        takingTime: [
        { required: true, message: "单价最近生效时间不能为空", trigger: "blur" }
      ] },
      }
    },
    props: {
      materialInfoId: {
        type: Number,
      },
      disabled: {
        type: Boolean,
        require: true
      }
    },
    watch: {
      materialInfoId(val) {
        if (isNullOrEmpty(val)) {
          this.reset()
        } else {
          this.getDetail(val)
        }
      }
    },
    created() {
      this.getDetail(this.materialInfoId)
    },
    methods: {
      /** 表单重置 */
      reset() {
        this.detail = {
          //附件列表
          attachmentList: [],
          id: null,
          materialNo: null,
          materialName: null,
          specs: null,
          unit: null,
          unitPrice: null,
          takingTime: null,
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

      /** 获取企业物料信息详情 */
      getDetail(materialInfoId) {
        if (isNullOrEmpty(materialInfoId)) {
          this.reset()
        } else {
          getMaterialInfo(materialInfoId).then(response => {
            this.detail = response.data
          });
        }
      },

      /** 提交按钮 */
      submitForm() {
        this.$refs["detail"].validate(valid => {
          if (valid) {
            if (this.detail.id != null) {
              updateMaterialInfo(this.detail).then(response => {
                if (response.code === 200) {
                  this.msgSuccess(response.msg)
                  this.handleClose()
                  this.handleQuery()
                }
              });
            } else {
              this.detail.companyId = this.$store.state.item.companyId
              this.detail.itemNo = this.$store.state.item.itemNo
              addMaterialInfo(this.detail).then(response => {
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
        if (this.materialInfoId) {
          this.getDetail(this.materialInfoId)
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
