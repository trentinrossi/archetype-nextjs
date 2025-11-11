'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { userSessionService } from '@/services/userSessionService';
import { CreateUserSessionRequest } from '@/types/user-session';
import { Input, Button } from '@/components/ui';

export default function CreateUserSessionPage() {
  const router = useRouter();
  const [formData, setFormData] = useState<CreateUserSessionRequest>({
    transactionId: '',
    programName: '',
    fromProgram: '',
    fromTransaction: '',
    programContext: undefined,
    reenterFlag: false,
  });
  const [loading, setLoading] = useState(false);
  const [errors, setErrors] = useState<Record<string, string>>({});

  const validateForm = (): boolean => {
    const newErrors: Record<string, string> = {};

    if (!formData.transactionId.trim()) {
      newErrors.transactionId = 'Transaction ID is required';
    } else if (formData.transactionId.length > 4) {
      newErrors.transactionId = 'Transaction ID must be 4 characters or less';
    }

    if (!formData.programName.trim()) {
      newErrors.programName = 'Program name is required';
    } else if (formData.programName.length > 8) {
      newErrors.programName = 'Program name must be 8 characters or less';
    }

    if (formData.fromProgram && formData.fromProgram.length > 8) {
      newErrors.fromProgram = 'From program must be 8 characters or less';
    }

    if (formData.fromTransaction && formData.fromTransaction.length > 4) {
      newErrors.fromTransaction = 'From transaction must be 4 characters or less';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateForm()) {
      return;
    }

    const submitData = {
      ...formData,
      fromProgram: formData.fromProgram || undefined,
      fromTransaction: formData.fromTransaction || undefined,
    };

    try {
      setLoading(true);
      await userSessionService.createUserSession(submitData);
      router.push('/admin/sessions');
    } catch (err) {
      alert('Failed to create session');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="p-6 max-w-2xl">
      <h1 className="text-2xl font-bold mb-6">Create New User Session</h1>
      
      <form onSubmit={handleSubmit} className="space-y-4">
        <div>
          <Input
            label="Transaction ID"
            value={formData.transactionId}
            onChange={(e) => {
              setFormData({ ...formData, transactionId: e.target.value.toUpperCase() });
              setErrors({ ...errors, transactionId: '' });
            }}
            required
            maxLength={4}
          />
          {errors.transactionId && (
            <p className="mt-1 text-sm text-red-600">{errors.transactionId}</p>
          )}
          <p className="mt-1 text-sm text-gray-500">Maximum 4 characters</p>
        </div>
        
        <div>
          <Input
            label="Program Name"
            value={formData.programName}
            onChange={(e) => {
              setFormData({ ...formData, programName: e.target.value.toUpperCase() });
              setErrors({ ...errors, programName: '' });
            }}
            required
            maxLength={8}
          />
          {errors.programName && (
            <p className="mt-1 text-sm text-red-600">{errors.programName}</p>
          )}
          <p className="mt-1 text-sm text-gray-500">Maximum 8 characters</p>
        </div>
        
        <div>
          <Input
            label="From Program (Optional)"
            value={formData.fromProgram}
            onChange={(e) => {
              setFormData({ ...formData, fromProgram: e.target.value.toUpperCase() });
              setErrors({ ...errors, fromProgram: '' });
            }}
            maxLength={8}
          />
          {errors.fromProgram && (
            <p className="mt-1 text-sm text-red-600">{errors.fromProgram}</p>
          )}
          <p className="mt-1 text-sm text-gray-500">Maximum 8 characters</p>
        </div>
        
        <div>
          <Input
            label="From Transaction (Optional)"
            value={formData.fromTransaction}
            onChange={(e) => {
              setFormData({ ...formData, fromTransaction: e.target.value.toUpperCase() });
              setErrors({ ...errors, fromTransaction: '' });
            }}
            maxLength={4}
          />
          {errors.fromTransaction && (
            <p className="mt-1 text-sm text-red-600">{errors.fromTransaction}</p>
          )}
          <p className="mt-1 text-sm text-gray-500">Maximum 4 characters</p>
        </div>
        
        <div>
          <Input
            label="Program Context (Optional)"
            type="number"
            value={formData.programContext || ''}
            onChange={(e) => {
              setFormData({ 
                ...formData, 
                programContext: e.target.value ? Number(e.target.value) : undefined 
              });
            }}
          />
          <p className="mt-1 text-sm text-gray-500">Numeric context value</p>
        </div>
        
        <div className="flex items-center">
          <input
            type="checkbox"
            id="reenterFlag"
            checked={formData.reenterFlag}
            onChange={(e) => setFormData({ ...formData, reenterFlag: e.target.checked })}
            className="h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded"
          />
          <label htmlFor="reenterFlag" className="ml-2 block text-sm text-gray-900">
            Reenter Flag
          </label>
        </div>
        
        <div className="flex gap-2 pt-4">
          <Button type="submit" disabled={loading}>
            {loading ? 'Creating...' : 'Create Session'}
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push('/admin/sessions')}
          >
            Cancel
          </Button>
        </div>
      </form>
    </div>
  );
}
