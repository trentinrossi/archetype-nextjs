'use client';

import React, { useState, useEffect } from 'react';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';
import { UserSecurityDTO, UpdateUserSecurityRequest } from '@/types/user';
import { userService } from '@/services/userService';

interface UserUpdateFormProps {
  preSelectedUserId?: string;
  onSuccess: (message: string) => void;
  onExit: () => void;
}

export function UserUpdateForm({ preSelectedUserId, onSuccess, onExit }: UserUpdateFormProps) {
  const [searchUserId, setSearchUserId] = useState(preSelectedUserId || '');
  const [currentUser, setCurrentUser] = useState<UserSecurityDTO | null>(null);
  const [formData, setFormData] = useState<UpdateUserSecurityRequest>({});
  const [originalData, setOriginalData] = useState<UserSecurityDTO | null>(null);
  const [message, setMessage] = useState('');
  const [loading, setLoading] = useState(false);

  // COUSR02C business rule: Auto-load user if pre-selected
  useEffect(() => {
    if (preSelectedUserId) {
      handleLookupUser();
    }
  }, [preSelectedUserId]);

  // COUSR02C business rule: Handle ENTER key (lookup user)
  const handleLookupUser = async () => {
    if (!searchUserId.trim()) {
      setMessage('User ID can NOT be empty...');
      return;
    }

    setLoading(true);
    setMessage('');

    try {
      const user = await userService.getUserById(searchUserId);
      setCurrentUser(user);
      setOriginalData(user);
      
      // Populate form with current user data
      setFormData({
        password: user.password,
        userType: user.userType,
        programName: user.programName,
        transactionId: user.transactionId,
        active: user.active
      });
      
      setMessage('Press PF5 key to save your updates ...');
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unable to lookup User...';
      setMessage(errorMessage);
      setCurrentUser(null);
      setOriginalData(null);
      setFormData({});
    } finally {
      setLoading(false);
    }
  };

  // COUSR02C business rule: Handle PF5 key (update user)
  const handleUpdate = async () => {
    if (!currentUser || !originalData) {
      setMessage('Please lookup a user first');
      return;
    }

    // Validate required fields (COUSR02C business rules)
    if (!formData.password?.trim()) {
      setMessage('Password can NOT be empty...');
      return;
    }

    if (!formData.userType) {
      setMessage('User Type can NOT be empty...');
      return;
    }

    setLoading(true);
    setMessage('');

    try {
      const response = await userService.updateUser(currentUser.userId, formData);
      setMessage(response.message);
      onSuccess(response.message);
      
      // Update current user data
      setCurrentUser(response.user);
      setOriginalData(response.user);
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unable to Update User...';
      setMessage(errorMessage);
    } finally {
      setLoading(false);
    }
  };

  // COUSR02C business rule: Handle PF3 key (save and exit)
  const handleSaveAndExit = async () => {
    await handleUpdate();
    if (!message.includes('error') && !message.includes('empty') && !message.includes('modify')) {
      setTimeout(() => onExit(), 1000);
    }
  };

  const handleInputChange = (field: keyof UpdateUserSecurityRequest) => (
    e: React.ChangeEvent<HTMLInputElement | HTMLSelectElement>
  ) => {
    const value = e.target.value;
    
    // COBOL field length constraints
    let processedValue = value;
    if (field === 'password') {
      processedValue = value.substring(0, 8);
    }
    
    setFormData(prev => ({
      ...prev,
      [field]: processedValue
    }));
    
    // Clear message when user makes changes
    if (message.includes('modify')) {
      setMessage('');
    }
  };

  // COUSR02C business rule: Clear form (PF4 key)
  const handleClear = () => {
    setSearchUserId('');
    setCurrentUser(null);
    setOriginalData(null);
    setFormData({});
    setMessage('');
  };

  // Handle invalid key press
  const handleInvalidKey = () => {
    setMessage('Invalid key pressed');
  };

  return (
    <div className="min-h-screen bg-gray-100 p-4">
      <div className="bg-white rounded-lg shadow-md p-6 max-w-2xl mx-auto">
        {/* Screen Header - COUSR02C layout */}
        <div className="mb-6 text-center border-b pb-4">
          <h1 className="text-2xl font-bold text-gray-800 mb-2">CARDDEMO</h1>
          <h2 className="text-lg text-gray-600 mb-4">Update User</h2>
          <div className="text-sm text-gray-500 space-y-1">
            <div>Program: COUSR02C | Transaction: CU02</div>
            <div>{new Date().toLocaleDateString('en-US', { 
              month: '2-digit', 
              day: '2-digit', 
              year: '2-digit' 
            })} {new Date().toLocaleTimeString('en-US', { 
              hour12: false 
            })}</div>
          </div>
        </div>

        {/* User Lookup Section */}
        <div className="mb-6 p-4 bg-gray-50 rounded">
          <div className="flex gap-2 items-end">
            <div className="flex-1">
              <Input
                label="User ID"
                type="text"
                value={searchUserId}
                onChange={(e) => setSearchUserId(e.target.value.toUpperCase().substring(0, 8))}
                maxLength={8}
                placeholder="Enter User ID to lookup"
                autoFocus={!preSelectedUserId}
              />
            </div>
            <Button
              onClick={handleLookupUser}
              disabled={loading}
              className="mb-0"
            >
              Lookup (ENTER)
            </Button>
          </div>
        </div>

        {/* User Update Form */}
        {currentUser && (
          <div className="space-y-4">
            <div className="p-4 bg-blue-50 rounded">
              <h3 className="font-medium text-blue-800 mb-2">Current User: {currentUser.userId}</h3>
              <div className="text-sm text-blue-600">
                Status: {currentUser.active ? 'Active' : 'Inactive'} | 
                Type: {currentUser.userTypeDisplayName}
              </div>
            </div>

            <Input
              label="Password"
              type="password"
              value={formData.password || ''}
              onChange={handleInputChange('password')}
              maxLength={8}
              placeholder="Enter new password (max 8 chars)"
              required
            />

            <div className="space-y-1">
              <label className="text-sm font-medium leading-none">
                User Type
              </label>
              <select
                value={formData.userType || ''}
                onChange={handleInputChange('userType')}
                className="flex h-10 w-full rounded-md border border-border bg-background px-3 py-2 text-sm ring-offset-background focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50"
                required
              >
                <option value="">Select User Type</option>
                <option value="GENERAL">General User</option>
                <option value="ADMIN">Administrator</option>
              </select>
            </div>

            <div className="grid grid-cols-2 gap-4">
              <Input
                label="Program Name"
                type="text"
                value={formData.programName || ''}
                onChange={handleInputChange('programName')}
                placeholder="Program Name"
              />

              <Input
                label="Transaction ID"
                type="text"
                value={formData.transactionId || ''}
                onChange={handleInputChange('transactionId')}
                placeholder="Transaction ID"
              />
            </div>

            <div className="flex items-center space-x-2">
              <input
                type="checkbox"
                id="active"
                checked={formData.active || false}
                onChange={(e) => setFormData(prev => ({ ...prev, active: e.target.checked }))}
                className="rounded border-border"
              />
              <label htmlFor="active" className="text-sm font-medium">
                Active User
              </label>
            </div>
          </div>
        )}

        {/* Message area */}
        {message && (
          <div className={`mt-4 p-3 rounded text-sm ${
            message.includes('updated') || message.includes('Press PF5')
              ? 'bg-green-100 text-green-700' 
              : message.includes('modify')
              ? 'bg-yellow-100 text-yellow-700'
              : 'bg-red-100 text-red-700'
          }`}>
            {message}
          </div>
        )}

        {/* Function Keys - COUSR02C business rules */}
        <div className="mt-6 space-y-2">
          {currentUser && (
            <div className="grid grid-cols-2 gap-2 mb-2">
              <Button
                onClick={handleSaveAndExit}
                disabled={loading}
                variant="secondary"
              >
                Save & Exit (PF3)
              </Button>

              <Button
                onClick={handleUpdate}
                disabled={loading}
              >
                Update (PF5)
              </Button>
            </div>
          )}

          <div className="grid grid-cols-3 gap-2">
            <Button
              variant="outline"
              onClick={onExit}
              disabled={loading}
            >
              Cancel (PF12)
            </Button>

            <Button
              variant="outline"
              onClick={handleClear}
              disabled={loading}
            >
              Clear (PF4)
            </Button>

            <Button
              variant="destructive"
              onClick={handleInvalidKey}
              disabled={loading}
            >
              Invalid Key
            </Button>
          </div>
        </div>

        {/* Instructions */}
        <div className="mt-6 text-xs text-gray-500 text-center space-y-1">
          <p>Enter User ID and press Lookup to load user data</p>
          <p>Modify fields as needed, then press Update (PF5) or Save & Exit (PF3)</p>
          <p>Use Clear (PF4) to start over | Cancel (PF12) to exit without saving</p>
        </div>
      </div>
    </div>
  );
}