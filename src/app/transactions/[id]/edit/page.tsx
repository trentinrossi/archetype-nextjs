'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { transactionService } from '@/services/transactionService';
import { Transaction, UpdateTransactionRequest } from '@/types/transaction';
import { Input, Button } from '@/components/ui';

export default function EditTransactionPage() {
  const params = useParams();
  const router = useRouter();
  const [formData, setFormData] = useState<UpdateTransactionRequest>({});
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [validationErrors, setValidationErrors] = useState<Record<string, string>>({});

  useEffect(() => {
    if (params.id) {
      fetchTransaction(params.id as string);
    }
  }, [params.id]);

  const fetchTransaction = async (id: string) => {
    try {
      setLoading(true);
      const data = await transactionService.getTransactionById(id);
      setFormData({
        tranTypeCd: data.tranTypeCd,
        tranCatCd: data.tranCatCd,
        tranSource: data.tranSource,
        tranDesc: data.tranDesc,
        tranAmt: data.tranAmt,
        tranCardNum: data.tranCardNum,
        tranMerchantId: data.tranMerchantId,
        tranMerchantName: data.tranMerchantName,
        tranMerchantCity: data.tranMerchantCity,
        tranMerchantZip: data.tranMerchantZip,
        tranOrigTs: data.tranOrigTs,
        tranProcTs: data.tranProcTs,
      });
    } catch (err) {
      setError('Failed to load transaction');
      console.error('Failed to load transaction:', err);
    } finally {
      setLoading(false);
    }
  };

  const validateForm = (): boolean => {
    const errors: Record<string, string> = {};

    if (formData.tranTypeCd !== undefined && !formData.tranTypeCd) {
      errors.tranTypeCd = 'Transaction Type Code is required';
    }

    if (formData.tranCatCd !== undefined && !formData.tranCatCd) {
      errors.tranCatCd = 'Transaction Category Code is required';
    }

    if (formData.tranSource && formData.tranSource.length > 10) {
      errors.tranSource = 'Transaction Source max 10 characters';
    }

    if (formData.tranDesc && formData.tranDesc.length > 60) {
      errors.tranDesc = 'Description max 60 characters';
    }

    if (formData.tranCardNum && formData.tranCardNum.length !== 16) {
      errors.tranCardNum = 'Card Number must be 16 digits';
    }

    if (formData.tranMerchantId && formData.tranMerchantId.length > 9) {
      errors.tranMerchantId = 'Merchant ID max 9 digits';
    }

    if (formData.tranMerchantName && formData.tranMerchantName.length > 30) {
      errors.tranMerchantName = 'Merchant Name max 30 characters';
    }

    if (formData.tranMerchantCity && formData.tranMerchantCity.length > 25) {
      errors.tranMerchantCity = 'Merchant City max 25 characters';
    }

    if (formData.tranMerchantZip && formData.tranMerchantZip.length > 10) {
      errors.tranMerchantZip = 'Merchant Zip max 10 characters';
    }

    setValidationErrors(errors);
    return Object.keys(errors).length === 0;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateForm()) {
      return;
    }

    try {
      setSaving(true);
      setError(null);
      await transactionService.updateTransaction(params.id as string, formData);
      router.push(`/transactions/${params.id}`);
    } catch (err: any) {
      setError(err.message || 'Failed to update transaction');
      console.error(err);
    } finally {
      setSaving(false);
    }
  };

  if (loading) {
    return <div className="p-6">Loading transaction...</div>;
  }

  if (error && !formData.tranTypeCd) {
    return (
      <div className="p-6">
        <div className="mb-4 p-4 bg-red-50 border border-red-200 text-red-700 rounded">
          {error}
        </div>
        <Button variant="secondary" onClick={() => router.push('/transactions')}>
          Back to List
        </Button>
      </div>
    );
  }

  return (
    <div className="p-6 max-w-4xl">
      <h1 className="text-2xl font-bold mb-6">Edit Transaction</h1>

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
              label="Card Number"
              value={formData.tranCardNum || ''}
              onChange={(e) => setFormData({ ...formData, tranCardNum: e.target.value })}
              error={validationErrors.tranCardNum}
              maxLength={16}
              placeholder="16 digits"
            />
            
            <Input
              label="Transaction Type Code"
              type="number"
              value={formData.tranTypeCd || ''}
              onChange={(e) => setFormData({ ...formData, tranTypeCd: Number(e.target.value) })}
              error={validationErrors.tranTypeCd}
            />
            
            <Input
              label="Transaction Category Code"
              type="number"
              value={formData.tranCatCd || ''}
              onChange={(e) => setFormData({ ...formData, tranCatCd: Number(e.target.value) })}
              error={validationErrors.tranCatCd}
            />
            
            <Input
              label="Transaction Source"
              value={formData.tranSource || ''}
              onChange={(e) => setFormData({ ...formData, tranSource: e.target.value })}
              error={validationErrors.tranSource}
              maxLength={10}
            />
            
            <Input
              label="Transaction Amount"
              type="number"
              step="0.01"
              value={formData.tranAmt || ''}
              onChange={(e) => setFormData({ ...formData, tranAmt: Number(e.target.value) })}
              error={validationErrors.tranAmt}
            />
            
            <div className="md:col-span-2">
              <Input
                label="Description"
                value={formData.tranDesc || ''}
                onChange={(e) => setFormData({ ...formData, tranDesc: e.target.value })}
                error={validationErrors.tranDesc}
                maxLength={60}
              />
            </div>
            
            <Input
              label="Original Date"
              type="date"
              value={formData.tranOrigTs || ''}
              onChange={(e) => setFormData({ ...formData, tranOrigTs: e.target.value })}
              error={validationErrors.tranOrigTs}
            />
            
            <Input
              label="Processing Date"
              type="date"
              value={formData.tranProcTs || ''}
              onChange={(e) => setFormData({ ...formData, tranProcTs: e.target.value })}
              error={validationErrors.tranProcTs}
            />
          </div>
        </div>

        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-lg font-semibold mb-4">Merchant Information</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <Input
              label="Merchant ID"
              value={formData.tranMerchantId || ''}
              onChange={(e) => setFormData({ ...formData, tranMerchantId: e.target.value })}
              error={validationErrors.tranMerchantId}
              maxLength={9}
            />
            
            <Input
              label="Merchant Name"
              value={formData.tranMerchantName || ''}
              onChange={(e) => setFormData({ ...formData, tranMerchantName: e.target.value })}
              error={validationErrors.tranMerchantName}
              maxLength={30}
            />
            
            <Input
              label="Merchant City"
              value={formData.tranMerchantCity || ''}
              onChange={(e) => setFormData({ ...formData, tranMerchantCity: e.target.value })}
              error={validationErrors.tranMerchantCity}
              maxLength={25}
            />
            
            <Input
              label="Merchant Zip"
              value={formData.tranMerchantZip || ''}
              onChange={(e) => setFormData({ ...formData, tranMerchantZip: e.target.value })}
              error={validationErrors.tranMerchantZip}
              maxLength={10}
            />
          </div>
        </div>
        
        <div className="flex gap-2 pt-4">
          <Button type="submit" disabled={saving}>
            {saving ? 'Saving...' : 'Save Changes'}
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push(`/transactions/${params.id}`)}
          >
            Cancel
          </Button>
        </div>
      </form>
    </div>
  );
}
