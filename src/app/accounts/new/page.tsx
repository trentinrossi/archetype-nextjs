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
    currentBalance: 0,
  });
  const [loading, setLoading] = useState(false);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (formData.accountId.length > 11) {
      alert('Account ID must be 11 characters or less');
      return;
    }
    
    if (formData.currentBalance < 0.01) {
      alert('Current balance must be at least $0.01');
      return;
    }
    
    try {
      setLoading(true);
      await accountService.createAccount(formData);
      router.push('/accounts');
    } catch (err: any) {
      alert(err.message || 'Failed to create account');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="p-6 max-w-2xl">
      <h1 className="text-2xl font-bold mb-6">Create New Account</h1>
      
      <form onSubmit={handleSubmit} className="space-y-4">
        <Input
          label="Account ID"
          value={formData.accountId}
          onChange={(e) => setFormData({ ...formData, accountId: e.target.value })}
          maxLength={11}
          required
          placeholder="Enter account ID (max 11 characters)"
        />
        
        <Input
          label="Current Balance"
          type="number"
          step="0.01"
          min="0.01"
          value={formData.currentBalance}
          onChange={(e) => setFormData({ ...formData, currentBalance: Number(e.target.value) })}
          required
          placeholder="Enter initial balance"
        />
        
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
