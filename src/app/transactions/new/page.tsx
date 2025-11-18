'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { transactionService } from '@/services/transactionService';
import { CreateTransactionRequest } from '@/types/transaction';
import { Input, Button } from '@/components/ui';

export default function CreateTransactionPage() {
  const router = useRouter();
  const [formData, setFormData] = useState<CreateTransactionRequest>({
    transactionId: '',
    tranTypeCd: 0,
    tranCatCd: 0,
    tranSource: '',
    tranDesc: '',
    tranAmt: 0,
    tranCardNum: '',
    tranMerchantId: '',
    tranMerchantName: '',
    tranMerchantCity: '',
    tranMerchantZip: '',
    tranOrigTs: '',
    tranProcTs: '',
  });
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [validationErrors, setValidationErrors] = useState<Record<string, string>>({});

  const validateForm = (): boolean => {
    const errors: Record<string, string> = {};

    if (!formData.transactionId || formData.transactionId.length !== 16) {
      errors.transactionId = 'Transaction ID must be 16 characters';
    }

    if (!formData.tranTypeCd) {
      errors.tranTypeCd = 'Transaction Type Code is required';
    }

    if (!formData.tranCatCd) {
      errors.tranCatCd = 'Transaction Category Code is required';
    }

    if (!formData.tranSource || formData.tranSource.length > 10) {
      errors.tranSource = 'Transaction Source is required (max 10 characters)';
    }

    if (!formData.tranDesc || formData.tranDesc.length > 60) {
      errors.tranDesc = 'Description is required (max 60 characters)';
    }

    if (!formData.tranAmt) {
      errors.tranAmt = 'Transaction Amount is required';
    }

    if (!formData.tranCardNum || formData.tranCardNum.length !== 16) {
      errors.tranCardNum = 'Card Number must be 16 digits';
    }

    if (!formData.tranMerchantId || formData.tranMerchantId.length > 9) {
      errors.tranMerchantId = 'Merchant ID is required (max 9 digits)';
    }

    if (!formData.tranMerchantName || formData.tranMerchantName.length > 30) {
      errors.tranMerchantName = 'Merchant Name is required (max 30 characters)';
    }

    if (!formData.tranMerchantCity || formData.tranMerchantCity.length > 25) {
      errors.tranMerchantCity = 'Merchant City is required (max 25 characters)';
    }

    if (!formData.tranMerchantZip || formData.tranMerchantZip.length > 10) {
      errors.tranMerchantZip = 'Merchant Zip is required (max 10 characters)';
    }

    if (!formData.tranOrigTs) {
      errors.tranOrigTs = 'Original Date is required (format: YYYY-MM-DD)';
    }

    if (!formData.tranProcTs) {
      errors.tranProcTs = 'Processing Date is required (format: YYYY-MM-DD)';
    }

    setValidationErrors(errors);
    return Object.keys(errors).length === 0;
  };

  const handleCopyLast = async () => {
    try {
      setLoading(true);
      setError(null);
      const lastTransaction = await transactionService.copyLastTransaction();
      setFormData({
        transactionId: '',
        tranTypeCd: lastTransaction.tranTypeCd,
        tranCatCd: lastTransaction.tranCatCd,
        tranSource: lastTransaction.tranSource,
        tranDesc: lastTransaction.tranDesc,
        tranAmt: lastTransaction.tranAmt,
        tranCardNum: lastTransaction.tranCardNum,
        tranMerchantId: lastTransaction.tranMerchantId,
        tranMerchantName: lastTransaction.tranMerchantName,
        tranMerchantCity: lastTransaction.tranMerchantCity,
        tranMerchantZip: lastTransaction.tranMerchantZip,
        tranOrigTs: lastTransaction.tranOrigTs,
        tranProcTs: lastTransaction.tranProcTs,
      });
    } catch (err) {
      setError('Failed to copy last transaction');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateForm()) {
      return;
    }

    try {
      setLoading(true);
      setError(null);
      const newTransaction = await transactionService.createTransaction(formData);
      router.push(`/transactions/${newTransaction.transactionId}`);
    } catch (err: any) {
      setError(err.message || 'Failed to create transaction');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleClear = () => {
    setFormData({
      transactionId: '',
      tranTypeCd: 0,
      tranCatCd: 0,
      tranSource: '',
      tranDesc: '',
      tranAmt: 0,
      tranCardNum: '',
      tranMerchantId: '',
      tranMerchantName: '',
      tranMerchantCity: '',
      tranMerchantZip: '',
      tranOrigTs: '',
      tranProcTs: '',
    });
    setValidationErrors({});
    setError(null);
  };

  return (
    <div className="p-6 max-w-4xl">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Add New Transaction</h1>
        <div className="flex gap-2">
          <Button 
            type="button" 
            variant="secondary" 
            onClick={handleCopyLast}
            disabled={loading}
          >
            Copy Last Transaction
          </Button>
        </div>
      </div>

      {error && (
        <div className="mb-4 p-4 bg-red-50 border border-red-200 text-red-700 rounded">
          {error}
        </div>
      )}
      
      <form onSubmit={handleSubmit} className="space-y-6">
        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-lg font-semibold mb-4">Transaction Information</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <Input
              label="Transaction ID"
              value={formData.transactionId}
              onChange={(e) => setFormData({ ...formData, transactionId: e.target.value })}
              error={validationErrors.transactionId}
              required
              maxLength={16}
              placeholder="16 characters"
            />
            
            <Input
              label="Card Number"
              value={formData.tranCardNum}
              onChange={(e) => setFormData({ ...formData, tranCardNum: e.target.value })}
              error={validationErrors.tranCardNum}
              required
              maxLength={16}
              placeholder="16 digits"
            />
            
            <Input
              label="Transaction Type Code"
              type="number"
              value={formData.tranTypeCd || ''}
              onChange={(e) => setFormData({ ...formData, tranTypeCd: Number(e.target.value) })}
              error={validationErrors.tranTypeCd}
              required
            />
            
            <Input
              label="Transaction Category Code"
              type="number"
              value={formData.tranCatCd || ''}
              onChange={(e) => setFormData({ ...formData, tranCatCd: Number(e.target.value) })}
              error={validationErrors.tranCatCd}
              required
            />
            
            <Input
              label="Transaction Source"
              value={formData.tranSource}
              onChange={(e) => setFormData({ ...formData, tranSource: e.target.value })}
              error={validationErrors.tranSource}
              required
              maxLength={10}
            />
            
            <Input
              label="Transaction Amount"
              type="number"
              step="0.01"
              value={formData.tranAmt || ''}
              onChange={(e) => setFormData({ ...formData, tranAmt: Number(e.target.value) })}
              error={validationErrors.tranAmt}
              required
            />
            
            <div className="md:col-span-2">
              <Input
                label="Description"
                value={formData.tranDesc}
                onChange={(e) => setFormData({ ...formData, tranDesc: e.target.value })}
                error={validationErrors.tranDesc}
                required
                maxLength={60}
              />
            </div>
            
            <Input
              label="Original Date"
              type="date"
              value={formData.tranOrigTs}
              onChange={(e) => setFormData({ ...formData, tranOrigTs: e.target.value })}
              error={validationErrors.tranOrigTs}
              required
            />
            
            <Input
              label="Processing Date"
              type="date"
              value={formData.tranProcTs}
              onChange={(e) => setFormData({ ...formData, tranProcTs: e.target.value })}
              error={validationErrors.tranProcTs}
              required
            />
          </div>
        </div>

        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-lg font-semibold mb-4">Merchant Information</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <Input
              label="Merchant ID"
              value={formData.tranMerchantId}
              onChange={(e) => setFormData({ ...formData, tranMerchantId: e.target.value })}
              error={validationErrors.tranMerchantId}
              required
              maxLength={9}
            />
            
            <Input
              label="Merchant Name"
              value={formData.tranMerchantName}
              onChange={(e) => setFormData({ ...formData, tranMerchantName: e.target.value })}
              error={validationErrors.tranMerchantName}
              required
              maxLength={30}
            />
            
            <Input
              label="Merchant City"
              value={formData.tranMerchantCity}
              onChange={(e) => setFormData({ ...formData, tranMerchantCity: e.target.value })}
              error={validationErrors.tranMerchantCity}
              required
              maxLength={25}
            />
            
            <Input
              label="Merchant Zip"
              value={formData.tranMerchantZip}
              onChange={(e) => setFormData({ ...formData, tranMerchantZip: e.target.value })}
              error={validationErrors.tranMerchantZip}
              required
              maxLength={10}
            />
          </div>
        </div>
        
        <div className="flex gap-2 pt-4">
          <Button type="submit" disabled={loading}>
            {loading ? 'Creating...' : 'Create Transaction'}
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={handleClear}
            disabled={loading}
          >
            Clear
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push('/transactions')}
          >
            Cancel
          </Button>
        </div>
      </form>
    </div>
  );
}
