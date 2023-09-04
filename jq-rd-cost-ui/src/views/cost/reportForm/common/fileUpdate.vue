<template>
  <div>
    <el-form ref="detail" :model="detail" :rules="rules" label-width="100px" :disabled="disabled">
      <el-row>
        <el-col :span="12">
          <el-form-item label="文件类型" prop="fileType">
            <el-select v-model="detail.fileType" placeholder="请选择文件类型">
              <el-option
                v-for="dict in fileTypeOptions"
                :key="dict.dictValue"
                :label="dict.dictLabel"
                :value="dict.dictValue"
              ></el-option>
            </el-select>
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="文件名称" prop="fileName">
            <el-input v-model="detail.fileName" placeholder="请输入文件名称" />
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="12">
          <el-form-item label="文件状态" prop="fileStatus">
            <el-select v-model="detail.fileStatus" placeholder="请选择文件状态">
              <el-option
                v-for="dict in fileStatusOptions"
                :key="dict.dictValue"
                :label="dict.dictLabel"
                :value="dict.dictValue"
              ></el-option>
            </el-select>
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="文件编号" prop="fileNo">
            <el-input v-model="detail.fileNo" placeholder="请输入文件编号" />
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
        <el-row>

        <el-col :span="12">
          <el-form-item label="生成材料" prop="fj1">
            <jq-attach
              v-model="detail.fileListfj1"
              relate-page="item_file"
              relate-key="fj1"
              :check-many="false"
              list-type="text"
              accept=".*"
              @changeAttachmentList="val => detail.fileListfj1 = val">
            </jq-attach>
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
  import {getFile, delFile, addFile, updateFile} from "@/api/cost/file";

  export default {
    name: 'fileUpdate',
    components: {},
    inject: ['handleQuery'],
    data() {
      return {
        fileTypeOptions: [],
        fileStatusOptions: [],
        detail: {},
        //表单校验
        rules: {
          companyId: [
            { required: true, message: "所属客户不能为空", trigger: "blur" }
          ],          delFlag: [
            { required: true, message: "逻辑删除不能为空", trigger: "blur" }
          ]        }
      }
    },
    props: {
      fileId: {
        type: Number,
      },
      disabled: {
        type: Boolean,
        require: true
      }
    },
    watch: {
      fileId(val) {
        if (isNullOrEmpty(val)) {
          this.reset()
        } else {
          this.getDetail(val)
        }
      }
    },
    created() {
      this.getDetail(this.fileId)
      this.getDicts("file_type").then(response => {
        this.fileTypeOptions = response.data;
      });
      this.getDicts("file_status").then(response => {
        this.fileStatusOptions = response.data;
      });
    },
    methods: {
      /** 表单重置 */
      reset() {
        this.detail = {
          //附件列表
          attachmentList: [],
          fileListfj1:[],
          id: null,
          fileType: null,
          fileName: null,
          fileStatus: null,
          fileNo: null,
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

      /** 获取汇总文件详情 */
      getDetail(fileId) {
        if (isNullOrEmpty(fileId)) {
          this.reset()
        } else {
          getFile(fileId).then(response => {
            this.detail = response.data
            this.detail.fileListfj1 = this.choseAttachByRelateKey(this.detail.attachmentList, "fj1");
          });
        }
      },

      /** 提交按钮 */
      submitForm() {
        this.$refs["detail"].validate(valid => {
          if (valid) {
            this.detail.attachmentList = [];
            this.detail.attachmentList = this.detail.attachmentList.concat(this.detail.fileListfj1);
            if (this.detail.id != null) {
              updateFile(this.detail).then(response => {
                if (response.code === 200) {
                  this.msgSuccess(response.msg)
                  this.handleClose()
                  this.handleQuery()
                }
              });
            } else {
              addFile(this.detail).then(response => {
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
        if (this.fileId) {
          this.getDetail(this.fileId)
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
