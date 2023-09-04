<template>
  <div>
    <el-form ref="detail" :model="detail" :rules="rules" label-width="100px" :disabled="disabled">
      <el-row>
        <el-col :span="12">
          <el-form-item label="科目编码" prop="code">
            <el-input v-model="detail.code" placeholder="请输入科目编码" />
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="会计科目" prop="name">
            <el-input v-model="detail.name" placeholder="请输入会计科目" />
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
  import {getConfAccountConfig, delConfAccountConfig, addConfAccountConfig, updateConfAccountConfig} from "@/api/conf/confAccountConfig";

  export default {
    name: 'confAccountConfigUpdate',
    components: {},
    inject: ['handleQuery'],
    data() {
      return {
        detail: {},
        //表单校验
        rules: {
          code: [
            { required: true, message: "科目编码不能为空", trigger: "blur" }
          ],
          name: [
            { required: true, message: "会计科目不能为空", trigger: "blur" }
          ]  }
      }
    },
    props: {
      confAccountConfigId: {
        type: Number,
      },
      disabled: {
        type: Boolean,
        require: true
      }
    },
    watch: {
      confAccountConfigId(val) {
        if (isNullOrEmpty(val)) {
          this.reset()
        } else {
          this.getDetail(val)
        }
      }
    },
    created() {
      this.getDetail(this.confAccountConfigId)
    },
    methods: {
      /** 表单重置 */
      reset() {
        this.detail = {
          //附件列表
          attachmentList: [],
          id: null,
          code: null,
          name: null,
          createBy: null,
          createTime: null,
          updateBy: null,
          updateTime: null,
          remark: null,
          delFlag: null
        }
        this.resetForm("detail");
      },

      /** 获取会计科目配置详情 */
      getDetail(confAccountConfigId) {
        if (isNullOrEmpty(confAccountConfigId)) {
          this.reset()
        } else {
          getConfAccountConfig(confAccountConfigId).then(response => {
            this.detail = response.data
          });
        }
      },

      /** 提交按钮 */
      submitForm() {
        this.$refs["detail"].validate(valid => {
          if (valid) {
            if (this.detail.id != null) {
              updateConfAccountConfig(this.detail).then(response => {
                if (response.code === 200) {
                  this.msgSuccess(response.msg)
                  this.handleClose()
                  this.handleQuery()
                }
              });
            } else {
              addConfAccountConfig(this.detail).then(response => {
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
        if (this.confAccountConfigId) {
          this.getDetail(this.confAccountConfigId)
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
