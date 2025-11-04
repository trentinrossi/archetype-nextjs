'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { CreateAccountRequest } from '@/types/account';
import { Input, Button } from '@/components/ui';

export default function CreateAccountPage() {
  const router = useRouter();
  const [formData, setFormData] = useState<CreateAccountRequest>({
    accountId: '',
    activeStatus: 'Y',
    currentBalance: 0,
    creditLimit: 0,
    cashCreditLimit: 0,
    openDate: new Date().toISOString().split('T')[0],
    expirationDate: '',
    reissueDate: '',
    currentCycleCredit: 0,
    currentCycleDebit: 0,
    addressZipCode: '',
    groupId: '',
  });
  const [loading, setLoading] = useState(false);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    try {
      setLoading(true);
      await accountService.createAccount(formData);
      router.push('/accounts');
    } catch (err) {
      alert('Failed to create account');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="p-6 max-w-4xl">
      <h1 className="text-2xl font-bold mb-6">Create New Account</h1>

      <form onSubmit={handleSubmit} className="space-y-4">
        <div className="grid grid-cols-2 gap-4">
          <Input
            label="Account ID"
            value={formData.accountId}
            onChange={(e) => setFormData({ ...formData, accountId: e.target.value })}
            required
            maxLength={11}
          />

          <Input
            label="Active Status"
            value={formData.activeStatus}
            onChange={(e) => setFormData({ ...formData, activeStatus: e.target.value })}
            required
            maxLength={1}
          />

          <Input
            label="Current Balance"
            type="number"
            step="0.01"
            value={formData.currentBalance}
            onChange={(e) => setFormData({ ...formData, currentBalance: Number(e.target.value) })}
            required
          />

          <Input
            label="Credit Limit"
            type="number"
            step="0.01"
            value={formData.creditLimit}
            onChange={(e) => setFormData({ ...formData, creditLimit: Number(e.target.value) })}
            required
          />

          <Input
            label="Cash Credit Limit"
            type="number"
            step="0.01"
            value={formData.cashCreditLimit}
            onChange={(e) => setFormData({ ...formData, cashCreditLimit: Number(e.target.value) })}
            required
          />

          <Input
            label="Group ID"
            value={formData.groupId}
            onChange={(e) => setFormData({ ...formData, groupId: e.target.value })}
            required
            maxLength={10}
          />

          <Input
            label="Open Date"
            type="date"
            value={formData.openDate}
            onChange={(e) => setFormData({ ...formData, openDate: e.target.value })}
            required
          />

          <Input
            label="Expiration Date"
            type="date"
            value={formData.expirationDate}
            onChange={(e) => setFormData({ ...formData, expirationDate: e.target.value })}
          />

          <Input
            label="Reissue Date"
            type="date"
            value={formData.reissueDate}
            onChange={(e) => setFormData({ ...formData, reissueDate: e.target.value })}
          />

          <Input
            label="Address ZIP Code"
            value={formData.addressZipCode}
            onChange={(e) => setFormData({ ...formData, addressZipCode: e.target.value })}
            maxLength={10}
          />

          <Input
            label="Current Cycle Credit"
            type="number"
            step="0.01"
            value={formData.currentCycleCredit}
            onChange={(e) => setFormData({ ...formData, currentCycleCredit: Number(e.target.value) })}
          />

          <Input
            label="Current Cycle Debit"
            type="number"
            step="0.01"
            value={formData.currentCycleDebit}
            onChange={(e) => setFormData({ ...formData, currentCycleDebit: Number(e.target.value) })}
          />
        </div>

        <div className="flex gap-2 pt-4">
          <Button type="submit" disabled={loading}>
            {loading ? 'Creating...' : 'Create Account'}
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push('/accounts')}
          >
            Cancel
          </Button>
        </div>
      </form>
    </div>
  );
}
