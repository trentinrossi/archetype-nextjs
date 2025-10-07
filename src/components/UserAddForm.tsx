'use client';

import React, { useState } from 'react';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';
import { CreateUserSecurityRequest } from '@/types/user';
import { userService } from '@/services/userService';

interface UserAddFormProps {
  onSuccess: (message: string) => void;
  onExit: () => void;
}

export function UserAddForm({ onSuccess, onExit }: UserAddFormProps) {
  const [formData, setFormData] = useState<CreateUserSecurityRequest>({
    userId: '',
    password: '',
    userType: 'GENERAL',
    programName: 'COSGN00C',
    transactionId: 'CC00',
    active: true
  });
  const [errors, setErrors] = useState<{ [key: string]: string }>({});
  const [message, setMessage] = useState('');
  const [loading, setLoading] = useState(false);

  // COUSR01C business rule: Handle form submission (ENTER key)
  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setLoading(true);
    setErrors({});
    setMessage('');

    try {
      const response = await userService.createUser(formData);
      
      // Clear form on success (COUSR01C business rule)
      setFormData({
        userId: '',
        password: '',
        userType: 'GENERAL',
        programName: 'COSGN00C',
        transactionId: 'CC00',
        active: true
      });
      
      setMessage(response.message);
      onSuccess(response.message);
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unable to Add User...';
      setMessage(errorMessage);
    } finally {
      setLoading(false);
    }
  };

  const handleInputChange = (field: keyof CreateUserSecurityRequest) => (
    e: React.ChangeEvent<HTMLInputElement | HTMLSelectElement>
  ) => {
    const value = e.target.value;
    
    // COBOL field length constraints
    let processedValue = value;
    if (field === 'userId') {
      processedValue = value.toUpperCase().substring(0, 8);
    } else if (field === 'password') {
      processedValue = value.substring(0, 8);
    }
    
    setFormData(prev => ({
      ...prev,
      [field]: processedValue
    }));
    
    // Clear field-specific errors
    if (errors[field]) {
      setErrors(prev => ({ ...prev, [field]: '' }));
    }
    
    // Clear general message
    if (message) {
      setMessage('');
    }
  };

  // COUSR01C business rule: Clear form (PF4 key)
  const handleClear = () => {
    setFormData({
      userId: '',
      password: '',
      userType: 'GENERAL',
      programName: 'COSGN00C',
      transactionId: 'CC00',
      active: true
    });
    setErrors({});
    setMessage('');
  };

  // Handle invalid key press
  const handleInvalidKey = () => {
    setMessage('INVALID KEY PRESSED');
  };

  return (
    <div className="min-h-screen bg-gray-100 p-4">
      <div className="bg-white rounded-lg shadow-md p-6 max-w-2xl mx-auto">
        {/* Screen Header - COUSR01C layout */}
        <div className="mb-6 text-center border-b pb-4">
          <h1 className="text-2xl font-bold text-gray-800 mb-2">CARDDEMO</h1>
          <h2 className="text-lg text-gray-600 mb-4">Add User</h2>
          <div className="text-sm text-gray-500 space-y-1">
            <div>Program: COUSR01C | Transaction: CU01</div>
            <div>{new Date().toLocaleDateString('en-US', { 
              month: '2-digit', 
              day: '2-digit', 
              year: '2-digit' 
            })} {new Date().toLocaleTimeString('en-US', { 
              hour12: false 
            })}</div>
          </div>
        </div>

        <form onSubmit={handleSubmit} className="space-y-4">
          <Input
            label="User ID"
            type="text"
            value={formData.userId}
            onChange={handleInputChange('userId')}
            maxLength={8}
            placeholder="Enter User ID (max 8 chars)"
            error={errors.userId}
            autoFocus
            required
          />

          <Input
            label="Password"
            type="password"
            value={formData.password}
            onChange={handleInputChange('password')}
            maxLength={8}
            placeholder="Enter Password (max 8 chars)"
            error={errors.password}
            required
          />

          <div className="space-y-1">
            <label className="text-sm font-medium leading-none">
              User Type
            </label>
            <select
              value={formData.userType}
              onChange={handleInputChange('userType')}
              className="flex h-10 w-full rounded-md border border-border bg-background px-3 py-2 text-sm ring-offset-background focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50"
              required
            >
              <option value="GENERAL">General User</option>
              <option value="ADMIN">Administrator</option>
            </select>
          </div>

          <div className="grid grid-cols-2 gap-4">
            <Input
              label="Program Name"
              type="text"
              value={formData.programName}
              onChange={handleInputChange('programName')}
              placeholder="Program Name"
            />

            <Input
              label="Transaction ID"
              type="text"
              value={formData.transactionId}
              onChange={handleInputChange('transactionId')}
              placeholder="Transaction ID"
            />
          </div>

          <div className="flex items-center space-x-2">
            <input
              type="checkbox"
              id="active"
              checked={formData.active}
              onChange={(e) => setFormData(prev => ({ ...prev, active: e.target.checked }))}
              className="rounded border-border"
            />
            <label htmlFor="active" className="text-sm font-medium">
              Active User
            </label>
          </div>

          {/* Message area */}
          {message && (
            <div className={`p-3 rounded text-sm ${
              message.includes('added') || message.includes('successful')
                ? 'bg-green-100 text-green-700' 
                : 'bg-red-100 text-red-700'
            }`}>
              {message}
            </div>
          )}

          {/* Function Keys - COUSR01C business rules */}
          <div className="space-y-2">
            <Button
              type="submit"
              className="w-full"
              disabled={loading}
            >
              {loading ? 'Adding User...' : 'Add User (ENTER)'}
            </Button>

            <div className="grid grid-cols-3 gap-2">
              <Button
                type="button"
                variant="secondary"
                onClick={onExit}
                disabled={loading}
              >
                Exit (PF3)
              </Button>

              <Button
                type="button"
                variant="outline"
                onClick={handleClear}
                disabled={loading}
              >
                Clear (PF4)
              </Button>

              <Button
                type="button"
                variant="destructive"
                onClick={handleInvalidKey}
                disabled={loading}
              >
                Invalid Key
              </Button>
            </div>
          </div>
        </form>

        {/* Instructions */}
        <div className="mt-6 text-xs text-gray-500 text-center space-y-1">
          <p>Fill in all required fields and press Add User</p>
          <p>Use Clear (PF4) to reset the form | Exit (PF3) to return to menu</p>
          <p>User ID will be converted to uppercase automatically</p>
        </div>
      </div>
    </div>
  );
}