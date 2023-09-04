<template>
  <div>
    <el-form ref="detail" :model="detail" :rules="rules" label-width="100px" :disabled="disabled">
      <el-row>
        <el-col :span="12">
          <el-form-item label="父ID" prop="parentId">
            <el-input v-model="detail.parentId" placeholder="请输入父ID" />
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="目录编号" prop="menuNo">
            <el-input v-model="detail.menuNo" placeholder="请输入目录编号" />
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="12">
          <el-form-item label="目录名称" prop="menuName">
            <el-input v-model="detail.menuName" placeholder="请输入目录名称" />
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="目录标识符" prop="menuKey">
            <el-input v-model="detail.menuKey" placeholder="请输入目录标识符" />
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="12">
          <el-form-item label="排序;从1开始 同阶层排序" prop="orderBy">
            <el-input v-model="detail.orderBy" placeholder="请输入排序;从1开始 同阶层排序" />
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="备注" prop="remark">
            <el-input v-model="detail.remark" placeholder="请输入备注" />
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
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
  import {getConfmenu, delConfmenu, addConfmenu, updateConfmenu} from "@/api/system/confmenu";

  export default {
    name: 'confmenuUpdate',
    components: {},
    inject: ['handleQuery'],
    data() {
      return {
        detail: {},
        //表单校验
        rules: {
          menuNo: [
            { required: true, message: "目录编号不能为空", trigger: "blur" }
          ],          menuName: [
            { required: true, message: "目录名称不能为空", trigger: "blur" }
          ],          delFlag: [
            { required: true, message: "逻辑删除不能为空", trigger: "blur" }
          ]        }
      }
    },
    props: {
      confmenuId: {
        type: Number,
      },
      disabled: {
        type: Boolean,
        require: true
      }
    },
    watch: {
      confmenuId(val) {
        if (isNullOrEmpty(val)) {
          this.reset()
        } else {
          this.getDetail(val)
        }
      }
    },
    created() {
      this.getDetail(this.confmenuId)
    },
    methods: {
      /** 表单重置 */
      reset() {
        this.detail = {
          //附件列表
          attachmentList: [],
          id: null,
          parentId: null,
          menuNo: null,
          menuName: null,
          menuKey: null,
          orderBy: null,
          createBy: null,
          createTime: null,
          updateBy: null,
          updateTime: null,
          remark: null,
          delFlag: null
        }
        this.resetForm("detail");
      },

      /** 获取整本材料目录配置详情 */
      getDetail(confmenuId) {
        if (isNullOrEmpty(confmenuId)) {
          this.reset()
        } else {
          getConfmenu(confmenuId).then(response => {
            this.detail = response.data
          });
        }
      },

      /** 提交按钮 */
      submitForm() {
        this.$refs["detail"].validate(valid => {
          if (valid) {
            if (this.detail.id != null) {
              updateConfmenu(this.detail).then(response => {
                if (response.code === 200) {
                  this.msgSuccess(response.msg)
                  this.handleClose()
                  this.handleQuery()
                }
              });
            } else {
              addConfmenu(this.detail).then(response => {
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
        if (this.confmenuId) {
          this.getDetail(this.confmenuId)
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
